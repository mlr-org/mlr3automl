train_auto = function(self, private, task) {
  pv = self$param_set$values
  large_data_set = task$nrow * task$ncol > pv$large_data_size
  n_workers = rush_config()$n_workers %??% 1L
  n_threads = pv$n_threads %??% 1L
  memory_limit = (pv$memory_limit %??% Inf) / n_workers
  autos = mlr_auto$mget(private$.learner_ids)

  lg$info("Training '%s' on task '%s'", self$id, task$id)

  # initialize mbo tuner
  tuner = tnr("async_mbo")

  # set number of workers
  if (large_data_set) {
    n_workers = max(1, floor(n_workers / 4L))
    n_threads = n_threads * 4L
    memory_limit = memory_limit * 4L

    tuner$param_set$set_values(n_workers = n_workers)
    lg$info("Large data set detected. Reducing number of workers to %i. Increasing number of threads to %i and memory limit to %i MB", n_workers, n_threads, memory_limit)
  }

  # resampling
  resampling = if (task$nrow < pv$small_data_size) {
    lg$info("Small data set detected. Using small data set resampling with %i iterations", pv$small_data_resampling$iters)
    pv$small_data_resampling
  } else {
    pv$resampling
  }

  # initialize graph learner
  if (pv$check_learners) {
    autos = keep(autos, function(auto) auto$check(task, memory_limit = memory_limit, large_data_set = large_data_set, devices = pv$devices))

    if (!length(autos)) {
      error_config("No learner is compatible with the task.")
    }
  }

  if (all(map_lgl(autos, function(auto) "hyperparameter-free" %in% auto$properties))) {
    error_config("All learners have no hyperparameters to tune. Combine with other learners.")
  }

  branches = map(autos, function(auto) auto$graph(task, pv$measure, n_threads, pv$learner_timeout, pv$devices))
  graph_learner = as_learner(po("branch", options = names(branches)) %>>%
    gunion(unname(branches)) %>>%
    po("unbranch", options = names(branches)), clone = TRUE)
  graph_learner$id = "graph_learner"
  graph_learner$predict_type = pv$measure$predict_type

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
  search_spaces = c(list(search_space), map(autos, function(auto) auto$search_space(task)))
  search_space = ps_union(unname(search_spaces))

  # add dependencies
  walk(autos, function(auto) {
    param_ids = auto$search_space(task)$ids()
    internal_tune_ids = auto$search_space(task)$ids(any_tags = "internal_tuning")
    param_ids = setdiff(param_ids, internal_tune_ids)

    walk(param_ids, function(param_id) {
      search_space$add_dep(
        id = param_id,
        on = "branch.selection",
        cond = CondEqual$new(auto$id)
      )
    })
  })

  callbacks = c(pv$callbacks, clbk("mlr3tuning.async_save_logs"))

  # tuning instance
  self$instance = ti_async(
    task = task,
    learner = graph_learner,
    resampling = resampling,
    measures = pv$measure,
    terminator = pv$terminator,
    search_space = search_space,
    callbacks = callbacks,
    store_benchmark_result = pv$store_benchmark_result,
    store_models = pv$store_models
  )

  if (pv$adaptive_design) {

    # start with best set design
    initial_design_best = if ("set" %in% pv$initial_design_type) {
      map_dtr(autos, function(auto) auto$design_set(task, measure = pv$measure, size = pv$initial_design_size), .fill = TRUE)
    }

    # adapt budget
    if (inherits(self$instance$terminator, "TerminatorEvals")) {
      budget = self$instance$terminator$param_set$values$n_evals
      self$instance$terminator$param_set$set_values(n_evals = floor(budget * pv$adaptive_design_fraction))
    } else if (inherits(self$instance$terminator, "TerminatorRunTime")) {
      start_time = Sys.time()
      budget = self$instance$terminator$param_set$values$secs
      self$instance$terminator$param_set$set_values(secs = floor(budget * pv$adaptive_design_fraction))
    } else {
      error_config("Unsupported terminator type for adaptive design.")
    }

    tnr_design_points = tnr("async_design_points", design = initial_design_best)
    tnr_design_points$optimize(self$instance)

    # if not terminated, continue with random search
    if (!self$instance$is_terminated) {

      if (inherits(self$instance$terminator, "TerminatorRunTime")) {
        self$instance$terminator$param_set$set_values(secs = max(0, floor(budget * pv$adaptive_design_fraction - difftime(Sys.time(), start_time, units = "secs"))))
      }

      tnr_random_search = tnr("async_random_search")
      tnr_random_search$optimize(self$instance)
    }

    # set remaining budget
    if (inherits(self$instance$terminator, "TerminatorEvals")) {
      self$instance$terminator$param_set$set_values(n_evals = budget)
    } else if (inherits(self$instance$terminator, "TerminatorRunTime")) {
      self$instance$terminator$param_set$set_values(secs = budget * (1 - pv$adaptive_design_fraction))
    }
  } else {
    initial_design_best = if ("set" %in% pv$initial_design_type) {
      map_dtr(autos, function(auto) auto$design_set(task, measure = pv$measure, size = pv$initial_design_size), .fill = TRUE)
    }
    initial_design_lhs = if ("lhs" %in% pv$initial_design_type) {
      map_dtr(autos, function(auto) auto$design_lhs(task, pv$initial_design_size), .fill = TRUE)
    }
    initial_design_random = if ("random" %in% pv$initial_design_type) {
      map_dtr(autos, function(auto) auto$design_random(task, pv$initial_design_size), .fill = TRUE)
    }
    initial_design_default = if ("default" %in% pv$initial_design_type) {
      map_dtr(autos, function(auto) auto$design_default(task), .fill = TRUE)
    }

    initial_designs = rbindlist(list(initial_design_best, initial_design_lhs, initial_design_random, initial_design_default), use.names = TRUE, fill = TRUE)
    initial_designs = initial_designs[sample(.N)]
    tuner$param_set$set_values(initial_design = initial_designs)
  }

  # configure tuner
  learner = lrn("regr.ranger",
      num.trees = 500L,
      se.method = "jack",
      splitrule = "variance",
      predict_type = "se",
      keep.inbag = TRUE,
      sample.fraction = 1,
      min.node.size = 3,
      min.bucket = 3,
      mtry.ratio = 5/6
    )

  tuner$surrogate =  srlrn(learner)
  tuner$surrogate$param_set$set_values(catch_errors = pv$encapsulate_mbo)

  if (!pv$encapsulate_mbo) {
    tuner$surrogate$learner$encapsulate(method = "none")
  }

  budget = 100L * search_space$length^2
  tuner$acq_function = acqf("stochastic_cb", lambda = 1.96, rate = 0.1, period = 25L)
  tuner$acq_optimizer = AcqOptimizerLocalSearch$new()
  tuner$acq_optimizer$param_set$set_values(n_searches = 10L, n_steps = ceiling(budget / 300L), n_neighs = 30L) #  catch_errors = pv$encapsulate_mbo

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
