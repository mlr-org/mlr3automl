train_auto = function(self, private, task) {
  pv = self$param_set$values
  large_data_set = task$nrow * task$ncol > pv$large_data_size
  n_workers = rush_config()$n_workers %??% 1L
  n_threads = if (large_data_set) 4L else pv$n_threads %??% 1L
  memory_limit = (pv$memory_limit %??% Inf) / n_workers
  autos = mlr_auto$mget(private$.learner_ids)

  lg$info("Training '%s' on task '%s'", self$id, task$id)

  # initialize mbo tuner
  tuner = tnr("async_mbo")

  # set number of workers
  if (large_data_set) {
    n_workers = max(1, floor(n_workers / 4L))
    tuner$param_set$set_values(n_workers = n_workers)
    lg$info("Large data set detected. Reducing number of workers to %i", n_workers)
  }

  # resampling
  resampling = if (task$nrow < pv$small_data_size) {
    lg$info("Small data set detected. Using small data set resampling with %i iterations", pv$small_data_resampling$iters)
    pv$small_data_resampling
  } else {
    pv$resampling
  }

  # initialize graph learner
  autos = keep(autos, function(auto) auto$check(task, memory_limit = memory_limit, large_data_set = large_data_set))

  if (!length(autos)) {
    error_config("No learner is compatible with the task.")
  }

  if (all(map_lgl(autos, function(auto) "hyperparameter-free" %in% auto$properties))) {
    error_config("All learners have no hyperparameters to tune. Combine with other learners.")
  }

  branches = map(autos, function(auto) auto$graph(task, pv$measure, n_threads, pv$learner_timeout))
  graph_learner = as_learner(po("branch", options = names(branches)) %>>%
    gunion(unname(branches)) %>>%
    po("unbranch", options = names(branches)), clone = TRUE)
  graph_learner$id = "graph_learner"
  graph_learner$predict_type = pv$measure$predict_type
  graph_learner$packages = c(graph_learner$packages, c("mlr3torch"))


  if (pv$encapsulate_learner) {
    fallback = lrn(sprintf("%s.featureless", task$task_type))
    fallback$predict_type = pv$measure$predict_type
    graph_learner$encapsulate(method = "mirai", fallback = fallback)
    graph_learner$timeout = c(train = pv$learner_timeout, predict = pv$learner_timeout)
  }

  learner_ids = map_chr(autos, "id")
  learners_with_validation = learner_ids[map_lgl(autos, function(auto) "internal_tuning" %in% auto$properties)]
  if (length(learners_with_validation)) {
    set_validate(graph_learner, "test", ids = learners_with_validation)
  }

  # initialize search space
  search_space = ps(
    branch.selection = p_fct(levels = learner_ids)
  )
  search_spaces = c(list(search_space), map(autos, function(auto) auto$search_space))
  search_space = ps_union(unname(search_spaces))

  # add dependencies
  walk(autos, function(auto) {
    param_ids = auto$search_space$ids()
    internal_tune_ids = auto$search_space$ids(any_tags = "internal_tuning")
    param_ids = setdiff(param_ids, internal_tune_ids)

    walk(param_ids, function(param_id) {
      search_space$add_dep(
        id = param_id,
        on = "branch.selection",
        cond = CondEqual$new(auto$id)
      )
    })
  })

  # tuning instance
  self$instance = ti_async(
    task = task,
    learner = graph_learner,
    resampling = resampling,
    measures = pv$measure,
    terminator = pv$terminator,
    search_space = search_space,
    callbacks = pv$callbacks,
    store_benchmark_result = pv$store_benchmark_result,
    store_models = pv$store_models
  )

  # initial design
  if (pv$initial_design_type == "lhs") {
    default_design = map_dtr(autos, function(auto) auto$design_default(task), .fill = TRUE)
    lhs_design = map_dtr(autos, function(auto) auto$design_lhs(task, pv$lhs_size), .fill = TRUE)
    initial_design = rbindlist(list(default_design, lhs_design), use.names = TRUE, fill = TRUE)
    setorderv(initial_design, "branch.selection")
    tuner$param_set$set_values(initial_design = initial_design)
  } else {

    if (!test_multi_class(pv$terminator, c("TerminatorEvals", "TerminatorRunTime"))) {
      error_config("Terminator must be a TerminatorEvals or TerminatorRunTime.")
    }

    size = if (inherits(pv$terminator, "TerminatorEvals")) {
      # set size of test design to 25% of the total number of evaluations
      lg$info("Evaluating initial design")

      ceiling(pv$terminator$param_set$values$n_evals * 0.25 / length(autos))
    } else if (inherits(pv$terminator, "TerminatorRunTime")) {
      # test runtime with one design point per learner
      lg$info("Evaluating runtime test design")
      1
    }

    test_design = map_dtr(autos, function(auto) auto$design_set(task, pv$measure, size), .fill = TRUE)
    test_tuner = tnr("async_design_points", design = test_design)
    test_tuner$optimize(self$instance)

    if (inherits(pv$terminator, "TerminatorRunTime")) {
      # increase size of test design to 25% of the total runtime
      runtime_learners = sum(self$instance$archive$data$runtime_learners, na.rm = TRUE)
      runtime_limit = pv$terminator$param_set$values$secs * 0.25
      size = floor(runtime_limit / runtime_learners * length(autos))

      if (size >= 1) {
        lg$info("Evaluating initial design")

        # draw additional design points for each learner but exclude already sampled rows
        test_design = pmap_dtr(list(autos, split(test_design, by = "branch.selection")), function(auto, design) auto$design_set(task, pv$measure, size, exclude = design, stratify = FALSE), .fill = TRUE)
        test_tuner = tnr("async_design_points", design = test_design)
        test_tuner$optimize(self$instance)
      } else {
        lg$info("Runtime test design already exceeded 25%% of the total runtime. Skipping initial design.")
      }
    }
  }

  # configure tuner
  tuner$surrogate = default_surrogate(self$instance, force_random_forest = TRUE)
  tuner$surrogate$param_set$set_values(catch_errors = pv$encapsulate_mbo)

  if (!pv$encapsulate_mbo) {
    tuner$surrogate$learner$encapsulate(method = "none")
  }

  tuner$acq_function = acqf("stochastic_cb", lambda = 1.96, rate = 0.1, period = 25L)
  tuner$acq_optimizer = acqo(
    optimizer = bbotk::opt("random_search", batch_size = 1000L),
    terminator = trm("evals", n_evals = 10000L),
    catch_errors = pv$encapsulate_mbo)

  # tune
  lg$info("Learner '%s' starts tuning phase", self$id)
  tuner$optimize(self$instance)

  # fit final model
  lg$info("Learner '%s' fits final model", self$id)


  if (length(learners_with_validation)) {
    set_validate(graph_learner, NULL, ids = learners_with_validation)
    # FIXME: remove this once we have a better way to handle this
    graph_learner$param_set$values$xgboost.callbacks = NULL
    graph_learner$param_set$values$lightgbm.callbacks = NULL
  }
  graph_learner$param_set$set_values(.values = self$instance$result_learner_param_vals, .insert = FALSE)
  graph_learner$timeout = c(train = Inf, predict = Inf)
  graph_learner$train(task)

  list(graph_learner = graph_learner, instance = self$instance)
}
