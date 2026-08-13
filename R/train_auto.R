train_auto = function(self, private, task) {
  pv = self$param_set$values
  if (is.null(pv$measure)) {
    pv$measure = default_measures(task$task_type)[[1L]]
    lg$info("No measure provided. Using default measure '%s'", pv$measure$id)
  }
  large_data_set = as.numeric(task$nrow) * task$ncol > pv$large_data_size
  # the workers are either a single group or distributed over the mirai compute profiles
  profiles = rush_config()$profiles
  n_workers = if (!is.null(profiles)) sum(profiles) else rush_config()$n_workers %??% 1L
  n_threads = pv$n_threads %??% 1L
  memory_limit = (pv$memory_limit %??% Inf) / n_workers
  autos = mlr_auto$mget(private$.learner_ids)

  # effective per-learner resource requirements decide which learners run on the gpu
  resources = effective_resources(autos, n_cpu = pv$n_cpu, n_gpu = pv$n_gpu, devices = pv$devices)
  uses_gpu = function(auto) resources$n_gpu[[auto$id]] > 0L
  # a learner that does not claim a gpu must neither be checked against nor configured for cuda
  learner_devices = function(auto) {
    if (uses_gpu(auto)) pv$devices else intersect(pv$devices, "cpu")
  }

  lg$info("Training '%s' on task '%s'", self$id, task$id)

  # the gpu learners keep the resources of a regular worker as long as their compute profile is not reduced
  n_threads_gpu = n_threads
  memory_limit_gpu = memory_limit

  # set number of workers
  if (large_data_set) {
    # the gpu profile is exempt from the large data set rules.
    # its number of workers is fixed by the number of gpus and its worker must not claim the cpu cores and the
    # memory that are freed on the cpu profile
    gpu_profile_exempt = "mlr3automl_gpu" %in% names(profiles)
    reduced = reduce_workers(profiles, n_workers)
    profiles = reduced$profiles
    n_workers = reduced$n_workers

    n_threads = as.integer(n_threads * reduced$scale)
    memory_limit = memory_limit * reduced$scale

    lg$info(
      # nolint next: line_length_linter
      "Large data set detected. Reducing number of workers to %i. Increasing number of threads to %i and memory limit to %.0f MB",
      n_workers,
      n_threads,
      memory_limit
    )

    if (gpu_profile_exempt) {
      lg$info(
        # nolint next: line_length_linter
        "Compute profile 'mlr3automl_gpu' is exempt. Keeping %i worker(s) with %i thread(s) and a memory limit of %.0f MB for the gpu learners",
        profiles[["mlr3automl_gpu"]],
        n_threads_gpu,
        memory_limit_gpu
      )
    } else {
      # without a gpu profile the gpu learners run on the reduced workers and receive their resources
      n_threads_gpu = n_threads
      memory_limit_gpu = memory_limit
    }
  }

  learner_n_threads = function(auto) if (uses_gpu(auto)) n_threads_gpu else n_threads
  learner_memory_limit = function(auto) if (uses_gpu(auto)) memory_limit_gpu else memory_limit

  # resampling
  # with bagging, a configuration is validated by the out-of-fold predictions of its children,
  # so the outer resampling trains on the complete data
  resampling = if (pv$bagging) {
    rsmp("insample")
  } else if (task$nrow < pv$small_data_size) {
    lg$info(
      "Small data set detected. Using small data set resampling with %i iterations",
      pv$small_data_resampling$iters
    )
    pv$small_data_resampling
  } else {
    pv$resampling
  }

  # initialize graph learner
  if (pv$check_learners) {
    autos = keep(autos, function(auto) {
      auto$check(
        task,
        memory_limit = learner_memory_limit(auto),
        large_data_set = large_data_set,
        devices = learner_devices(auto)
      )
    })

    if (!length(autos)) {
      error_config("No learner is compatible with the task.")
    }
  }

  if (all(map_lgl(autos, function(auto) "hyperparameter-free" %in% auto$properties))) {
    error_config("All learners have no hyperparameters to tune. Combine with other learners.")
  }

  # initialize mbo tuner
  # mixed gpu requirements are optimized on a cpu and a gpu subspace, each running on its own compute profile
  gpu_ids = names(keep(autos, uses_gpu))
  cpu_ids = setdiff(names(autos), gpu_ids)
  mixed_devices = length(gpu_ids) > 0L && length(cpu_ids) > 0L
  # the workers are divided among the subspaces by the compute profiles, so every subspace needs its own profile
  use_subspaces = mixed_devices && setequal(names(profiles), c("mlr3automl_cpu", "mlr3automl_gpu"))

  if (mixed_devices && !use_subspaces) {
    if (is.null(profiles)) {
      lg$info("No mirai compute profiles are set up. Optimizing cpu and gpu learners in a single search space")
    } else {
      lg$info(
        # nolint next: line_length_linter
        "Compute profiles %s do not match the profiles 'mlr3automl_cpu' and 'mlr3automl_gpu' of the cpu and gpu subspace. Optimizing cpu and gpu learners in a single search space",
        str_collapse(names(profiles), quote = "'")
      )
    }
  }

  tuner = if (use_subspaces) tnr("adbo_subspaces") else tnr("async_mbo")
  if (large_data_set) {
    # the reduced number of workers is passed to the tuner because the rush plan still holds the original number
    if (!is.null(profiles)) {
      tuner$param_set$set_values(profiles = profiles)
    } else {
      tuner$param_set$set_values(n_workers = n_workers)
    }
  }

  branches = map(autos, function(auto) {
    if (pv$bagging) {
      # the bagged graph divides `learner_timeout` among the child models itself
      auto$graph_bagged(
        task,
        pv$measure,
        learner_n_threads(auto),
        pv$learner_timeout,
        learner_devices(auto),
        pv$bagging_folds
      )
    } else {
      auto$graph(task, pv$measure, learner_n_threads(auto), pv$learner_timeout, learner_devices(auto))
    }
  })
  graph_learner = as_learner(
    po("branch", options = names(branches)) %>>%
      gunion(unname(branches)) %>>%
      po("unbranch", options = names(branches)),
    clone = TRUE
  )
  graph_learner$id = "graph_learner"
  # honor both the measure requirement and the user-requested predict type
  predict_type = highest_predict_type(task$task_type, c(pv$measure$predict_type, self$predict_type))
  graph_learner$predict_type = predict_type

  if (pv$encapsulate_learner) {
    fallback = lrn(sprintf("%s.featureless", task$task_type))
    fallback$predict_type = predict_type
    graph_learner$encapsulate(method = "mirai", fallback = fallback)
    graph_learner$timeout = c(train = pv$learner_timeout, predict = pv$learner_timeout)
  }

  learner_ids = map_chr(autos, "id")
  learners_with_validation = learner_ids[map_lgl(autos, function(auto) "internal_tuning" %in% auto$properties)]
  # nolint next: object_length_linter
  learners_without_hyperparameters = learner_ids[map_lgl(autos, function(auto) {
    "hyperparameter-free" %in% auto$properties
  })]

  if (pv$bagging) {
    # the validate field must be set for the worker to extract the internal valid scores,
    # but the bagged pipeops create their validation data internally and discard the incoming validation task.
    # set_validate() must not be used because it dispatches on the bagged pipeops
    graph_learner$validate = "test"
    # the out-of-fold score replaces the prediction-based score, so no predictions are needed during tuning
    graph_learner$predict_sets = NULL
  } else if (length(learners_with_validation)) {
    set_validate(graph_learner, "test", ids = learners_with_validation)
  }

  # initialize search space
  search_space = combine_search_spaces(autos, task)

  # with bagging, failed configurations produce no internal valid score,
  # so the missing scores are imputed with a penalized featureless baseline score
  score_penalty = if (pv$bagging) {
    featureless = lrn(sprintf("%s.featureless", task$task_type))
    featureless$predict_type = predict_type
    baseline = featureless$train(task)$predict(task)$score(pv$measure, task = task)
    nudge = 0.01 * max(abs(baseline), 1)
    unname(if (pv$measure$minimize) baseline + nudge else baseline - nudge)
  }

  callbacks = c(
    pv$callbacks,
    clbk("mlr3tuning.async_save_logs"),
    clbk("mlr3automl.initial_design_runtime", initial_design_fraction = pv$initial_design_fraction),
    if (pv$bagging) clbk("mlr3automl.impute_valid_score", penalty = score_penalty),
    # reuse a persistent mirai daemon per worker for the "mirai" encapsulation of the learners
    if (pv$encapsulate_learner) clbk("mlr3automl.encapsulation_daemon")
  )

  # with bagging, the tuner optimizes the out-of-fold score reported as internal valid score
  tuning_measure = if (pv$bagging) {
    msr("internal_valid_score", minimize = pv$measure$minimize)
  } else {
    pv$measure
  }

  # tuning instance
  self$instance = ti_async(
    task = task,
    learner = graph_learner,
    resampling = resampling,
    measures = tuning_measure,
    terminator = pv$terminator,
    search_space = search_space,
    callbacks = callbacks,
    store_benchmark_result = pv$store_benchmark_result,
    store_models = pv$store_models,
    rush = self$rush
  )

  # initial design
  initial_design_default = if (pv$initial_design_default) {
    map_dtr(autos, function(auto) auto$design_default(task), .fill = TRUE)
  }

  initial_design_set = if (pv$initial_design_set) {
    map_dtr(
      autos,
      function(auto) auto$design_set(task, measure = pv$measure, size = pv$initial_design_set),
      .fill = TRUE
    )
  }

  initial_design = if (!is.null(pv$initial_design_type) && pv$initial_design_size) {
    autos_with_hyperparameters = autos[!map_lgl(autos, function(auto) "hyperparameter-free" %in% auto$properties)]
    # nolint next: object_length_linter
    search_space_with_hyperparameters = combine_search_spaces(autos_with_hyperparameters, task)
    generate_initial_design(pv$initial_design_type, search_space_with_hyperparameters, pv$initial_design_size)
  }

  # add learners without hyperparameters to initial design
  if (!pv$initial_design_default && length(learners_without_hyperparameters)) {
    initial_design_default = map_dtr(
      autos[learners_without_hyperparameters],
      function(auto) auto$design_default(task),
      .fill = TRUE
    )
  }

  initial_designs = rbindlist(
    list(initial_design_default, initial_design_set, initial_design),
    use.names = TRUE,
    fill = TRUE
  )
  lg$info("Initial design size: %i", nrow(initial_designs))

  if (use_subspaces) {
    # the instance moves the internal-tuning parameters into its internal search space,
    # so the subspaces must partition the search space of the instance and not the combined one
    subspaces = partition_search_space(
      self$instance$search_space,
      param = "branch.selection",
      groups = list(cpu = cpu_ids, gpu = gpu_ids)
    )
    split_design = function(ids) {
      if (nrow(initial_designs)) initial_designs[branch.selection %in% ids] else initial_designs
    }
    # the cpu and the gpu subspace each run on their own mirai compute profile
    subspace_profiles = c(cpu = "mlr3automl_cpu", gpu = "mlr3automl_gpu")
    tuner$param_set$set_values(
      subspaces = subspaces,
      subspace_profiles = subspace_profiles,
      initial_design_subspace = list(cpu = split_design(cpu_ids), gpu = split_design(gpu_ids))
    )
    subspace_ids = c("cpu", "gpu")
    lg$info(
      "Optimizing %s",
      str_collapse(sprintf(
        "subspace '%s' (%s) with %i worker(s) on compute profile '%s'",
        subspace_ids,
        c(str_collapse(cpu_ids), str_collapse(gpu_ids)),
        profiles[subspace_profiles[subspace_ids]],
        subspace_profiles[subspace_ids]
      ))
    )
  } else {
    tuner$param_set$set_values(initial_design = initial_designs)
  }

  # configure tuner
  tuner$surrogate = default_surrogate(self$instance)
  tuner$surrogate$param_set$set_values(catch_errors = pv$encapsulate_mbo)

  if (!pv$encapsulate_mbo) {
    tuner$surrogate$learner$encapsulate(method = "none")
  }

  # tune
  lg$info("Learner '%s' starts tuning phase", self$id)
  tuner$optimize(self$instance)

  # fit final model
  lg$info("Learner '%s' fits final model", self$id)

  if (pv$bagging) {
    graph_learner$validate = NULL
    graph_learner$predict_sets = "test"
  } else if (length(learners_with_validation)) {
    set_validate(graph_learner, NULL, ids = learners_with_validation)
  }
  graph_learner$param_set$set_values(.values = self$instance$result_learner_param_vals, .insert = FALSE)
  walk(autos, function(auto) auto$finalize_model(graph_learner))

  # learners with the `"bagging_refit"` property deploy a single model instead of the ensemble.
  # the tuned values of the ensemble are already set, so the model only has to be trained on the complete data
  winner = self$instance$result$branch.selection
  if (pv$bagging && "bagging_refit" %in% autos[[winner]]$properties) {
    lg$info("Learner '%s' fits a single final model instead of the bagged ensemble", winner)
    graph_learner$param_set$set_values(.values = set_names(list(TRUE), sprintf("%s.bagging.refit", winner)))
  }

  # encapsulation set via LearnerAuto$encapsulate() applies to the final model fit only
  final_method = private$.encapsulation_method %??% "none"
  if (final_method == "none") {
    graph_learner$timeout = c(train = Inf, predict = Inf)
    graph_learner$encapsulate(method = "none")
  } else {
    graph_learner$timeout = self$timeout
    graph_learner$encapsulate(
      method = final_method,
      fallback = private$.encapsulation_fallback$clone(deep = TRUE),
      when = private$.encapsulation_when
    )
  }
  graph_learner$train(task)

  list(graph_learner = graph_learner, instance = self$instance)
}
