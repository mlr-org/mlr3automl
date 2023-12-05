learners = list(
  classif = list(
    rpart = lrn("classif.rpart", id = "rpart"),
    ranger = lrn("classif.ranger", id = "ranger"),
    xgboost = lrn("classif.xgboost", id = "xgboost"),
    glmnet = lrn("classif.glmnet", id = "glmnet"),
    kknn = lrn("classif.kknn", id = "kknn")
  ),
  regr = list(
    rpart = lrn("regr.rpart", id = "rpart"),
    ranger = lrn("regr.ranger", id = "ranger"),
    xgboost = lrn("regr.xgboost", id = "xgboost"),
    glmnet = lrn("regr.glmnet", id = "glmnet"),
    kknn = lrn("regr.kknn", id = "kknn")
  )
)

tuning_spaces = list(
  classif = list(
    rpart = lts("classif.rpart.default"),
    ranger = lts("classif.ranger.default"),
    xgboost = lts("classif.xgboost.default"),
    glmnet = lts("classif.glmnet.default"),
    kknn = lts("classif.kknn.default")
  ),
  regr = list(
    rpart = lts("regr.rpart.default"),
    ranger = lts("regr.ranger.default"),
    xgboost = lts("regr.xgboost.default"),
    glmnet = lts("regr.glmnet.default"),
    kknn = lts("regr.kknn.default")
  )
)

get_branch_pipeline = function(task_type) {
  learners = learners[[task_type]]

  # create branch
  graph = ppl("branch", graphs = learners)
  graph
}

get_search_space = function(task_type) {
  learners = learners[[task_type]]
  tuning_spaces = tuning_spaces[[task_type]]

  # add tuning spaces
  walk(names(learners), function(id) {
    tuning_space = tuning_spaces[[id]]
    learners[[id]]$param_set$set_values(.values = tuning_space$values)
  })

  # create branch
  graph = ppl("branch", graphs = learners)
  graph$param_set$set_values(branch.selection = to_tune(names(learners)))
  graph

  # create search space
  search_space = graph$param_set$search_space()

  # add dependencies
  walk(names(learners), function(learner_id) {
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
