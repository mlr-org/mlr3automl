learner_ids = c("rpart", "ranger", "xgboost", "glmnet", "kknn")

get_branch_pipeline = function(task_type) {
  learners = set_names(map(learner_ids, function(id) lrn(sprintf("%s.%s", task_type, id), id = id)), learner_ids)

  # set defaults
  learners$xgboost$param_set$set_values(nrounds = 25L)
  learners$ranger$param_set$set_values(num.trees = 250L)
  set_threads(learners, n = 8)

  # create branch
  graph = ppl("branch", graphs = learners)
  graph
}

get_search_space = function(task_type) {
  learners = set_names(map(learner_ids, function(id) lrn(sprintf("%s.%s", task_type, id), id = id)), learner_ids)
  tuning_spaces = set_names(map(learner_ids, function(id) lts(sprintf("%s.%s.automl", task_type, id))), learner_ids)

  # add tuning spaces
  walk(learner_ids, function(id) {
    tuning_space = tuning_spaces[[id]]
    learners[[id]]$param_set$set_values(.values = tuning_space$values)
  })

  # create branch
  graph = ppl("branch", graphs = learners)
  graph$param_set$set_values(branch.selection = to_tune(learner_ids))
  graph

  # create search space
  search_space = graph$param_set$search_space()

  # add dependencies
  walk(learner_ids, function(learner_id) {
    param_ids = search_space$ids()
    param_ids = grep(learner_id, param_ids, value = TRUE)
    walk(param_ids, function(param_id) {
      search_space$add_dep(
        id = param_id,
        on = "branch.selection",
        cond = CondEqual$new(learner_id)
      )
    })
  })

  search_space
}

generate_initial_design = function(task_type, task) {
  xdt = map_dtr(learner_ids, function(learner_id) {
    learner = lrn(sprintf("%s.%s", task_type, learner_id))
    tuning_space = lts(sprintf("%s.%s.automl", task_type, learner_id))
    learner$param_set$set_values(.values = tuning_space$values)
    search_space = learner$param_set$search_space()
    xss = default_values(learner, search_space = search_space, task = task)
    has_logscale = map_lgl(search_space$params, function(param) get_private(param)$.has_logscale)
    xdt = as.data.table(map_if(xss, has_logscale, function(value) if (value > 0) log(value) else value))
    setnames(xdt, sprintf("%s.%s", learner_id, names(xdt)))
    xdt[, branch.selection := learner_id]
  }, .fill = TRUE)

  # add xgboost
  learner = lrn(sprintf("%s.xgboost", task_type))
  tuning_space = lts(sprintf("%s.xgboost.automl", task_type))
  learner$param_set$set_values(.values = tuning_space$values)
  search_space_xgboost = learner$param_set$search_space()

  xdt_xgboost = generate_design_random(search_space_xgboost, 5)$data
  setnames(xdt_xgboost, sprintf("%s.%s", "xgboost", names(xdt_xgboost)))
  xdt_xgboost[, branch.selection := "xgboost"]

  rbindlist(list(xdt, xdt_xgboost), fill = TRUE)
}


default_surrogate = function(instance = NULL, learner = NULL, n_learner = NULL, search_space = NULL, noisy = NULL) {
  assert_r6(instance, "OptimInstance", null.ok = TRUE)
  assert_r6(learner, "Learner", null.ok = TRUE)
  if (is.null(instance)) {
    noisy = assert_flag(noisy)
    search_space = assert_param_set(search_space)
    assert_int(n_learner, lower = 1L)
  } else {
    noisy = "noisy" %in% instance$objective$properties
    search_space = instance$search_space
    assert_int(n_learner, lower = 1L, null.ok = TRUE)
  }

  if (is.null(learner)) {
    is_mixed_space = !all(search_space$class %in% c("ParamDbl", "ParamInt"))
    has_deps = nrow(search_space$deps) > 0L
    require_namespaces("mlr3learners")
    if (!is_mixed_space) {
      require_namespaces("DiceKriging")
      learner = mlr3learners::LearnerRegrKM$new()
      learner$param_set$values = insert_named(
        learner$param_set$values,
        list(covtype = "matern3_2", optim.method = "gen", control = list(trace = FALSE))
      )
      if (noisy) {
        learner$param_set$values = insert_named(learner$param_set$values, list(nugget.estim = TRUE, jitter = 1e-12))
      } else {
        learner$param_set$values = insert_named(learner$param_set$values, list(nugget.stability = 10^-8))
      }
    } else {
      require_namespaces("ranger")
      learner = mlr3learners::LearnerRegrRanger$new()
      learner$param_set$values = insert_named(
        learner$param_set$values,
        list(num.trees = 500L, keep.inbag = TRUE, se.method = "jack")
      )
    }
    # Stability: evaluate and add a fallback
    learner$encapsulate[c("train", "predict")] = "evaluate"
    require_namespaces("ranger")
    fallback = mlr3learners::LearnerRegrRanger$new()
    fallback$param_set$values = insert_named(
      fallback$param_set$values,
      list(num.trees = 20L, keep.inbag = TRUE, se.method = "jack")
    )
    learner$fallback = fallback

    if (has_deps) {
      require_namespaces("mlr3pipelines")
      learner = mlr3pipelines::GraphLearner$new(
        mlr3pipelines::"%>>%"(
          mlr3pipelines::"%>>%"(
            mlr3pipelines::po("imputesample", affect_columns = mlr3pipelines::selector_type("logical")),
              mlr3pipelines::"%>>%"(
                mlr3pipelines::po("imputeoor", multiplier = 3, affect_columns = mlr3pipelines::selector_type(c("integer", "numeric", "character", "factor", "ordered"))),
                mlr3pipelines::po("colapply", applicator = as.factor, affect_columns = mlr3pipelines::selector_type("character"))
              )
          ),
          learner
        )
      )
      learner$encapsulate[c("train", "predict")] = "evaluate"
      learner$fallback = LearnerRegrFeatureless$new()
    }
  }

  if (is.null(n_learner)) n_learner = length(instance$archive$cols_y)
  if (n_learner == 1L) {
    SurrogateLearner$new(learner)
  } else  {
    learners = replicate(n_learner, learner$clone(deep = TRUE), simplify = FALSE)
    SurrogateLearnerCollection$new(learners)
  }
}

