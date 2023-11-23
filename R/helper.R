# get default search space
default_space = function(learner_list, task_type) {
  sp = ParamSet$new()
  # set params
  for (learner in learner_list) {
    for (param in learner_params[[learner]]) {
      p = param$clone(deep = TRUE)
      p$id = paste(task_type, p$id, sep='.')
      sp$add(p)
    }
  }

  # set tune tokens
  for (learner in learner_list) {
    for (token_name in names(learner_tokens[[learner]])) {
      p = learner_tokens[[learner]][token_name]
      names(p) = paste(task_type, token_name, sep = '.')
      sp$set_values(.values = p)
    }
  }

  # add dependencies for branch selection
  sp = add_branch_selection_dependencies(learner_list, sp, task_type)

  return(sp)
}


# add dependencies for branches
add_branch_selection_dependencies = function(learner_list, param_set, task_type) {
  learners_tune = paste(task_type, learner_list, sep = '.')
  param_set$add(ParamFct$new("branch.selection", learners_tune))
  param_set$set_values(.values = list(branch.selection = to_tune(learners_tune)))
  for (learner in learners_tune) {
    for (param in names(param_set$values)[grepl(learner, names(param_set$values))]) {
      param_set$add_dep(param, "branch.selection",
                        CondEqual$new(learner))
    }
  }
  return(param_set)
}

learners_default = c("glmnet", "kknn", "ranger", "rpart", "xgboost")
learner_params = list(
  'glmnet' = list(
    'glmnet.s' = ParamDbl$new(id = 'glmnet.s', lower = 0, upper = Inf, default = 0.01),
    'glmnet.alpha' = ParamDbl$new(id = 'glmnet.alpha', lower = 0, upper = 1, default = 1)
  ),
  'kknn' = list(
    'kknn.k' = ParamInt$new(id = 'kknn.k', lower = 1, upper = Inf, default = 7),
    'kknn.distance' = ParamDbl$new(id = 'kknn.distance', lower = 1, upper = Inf, default = 2),
    'kknn.kernel' = ParamFct$new(id = 'kknn.kernel', levels = c("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian",
                                                                "rank", "optimal"), default = "optimal")
  ),
  'ranger' = list(
    'ranger.mtry.ratio' = ParamDbl$new(id = 'ranger.mtry.ratio', lower = 0, upper = 1),
    'ranger.replace' = ParamLgl$new(id = 'ranger.replace', default = TRUE),
    'ranger.sample.fraction' = ParamDbl$new(id = 'ranger.sample.fraction', lower = 0, upper = 1),
    'ranger.num.trees' = ParamInt$new(id = 'ranger.num.trees', lower = 1, upper = Inf, default = 500)
  ),
  'rpart' = list(
    'rpart.minsplit' = ParamInt$new(id = 'rpart.minsplit', lower = 1, upper = Inf, default = 20),
    'rpart.minbucket' = ParamInt$new(id = 'rpart.minbucket', lower = 1, upper = Inf),
    'rpart.cp' = ParamDbl$new(id = 'rpart.cp', lower = 0, upper = 1, default = 0.01)
  ),
  'xgboost' = list(
    'xgboost.eta' = ParamDbl$new(id = 'xgboost.eta', lower = 0, upper = 1, default = 0.3),
    'xgboost.nrounds' = ParamInt$new(id = 'xgboost.nrounds', lower = 1, upper = Inf),
    'xgboost.max_depth' = ParamInt$new(id = 'xgboost.max_depth', lower = 0, upper = Inf, default = 6),
    'xgboost.colsample_bytree' = ParamDbl$new(id = 'xgboost.colsample_bytree', lower = 0, upper = 1, default = 1),
    'xgboost.colsample_bylevel' = ParamDbl$new(id = 'xgboost.colsample_bylevel', lower = 0, upper = 1, default = 1),
    'xgboost.lambda' = ParamDbl$new(id = 'xgboost.lambda', lower = 0, upper = Inf, default = 1),
    'xgboost.alpha' = ParamDbl$new(id = 'xgboost.alpha', lower = 0, upper = Inf, default = 0),
    'xgboost.subsample' = ParamDbl$new(id = 'xgboost.subsample', lower = 0, upper = 1, default = 1)
  )
)
learner_tokens = list(
  'glmnet' = list(
    'glmnet.s' = to_tune(1e-04, 10000, logscale = TRUE),
    'glmnet.alpha' = to_tune(0, 1)
  ),
  'kknn' = list(
    'kknn.k' = to_tune(1, 50, logscale = TRUE),
    'kknn.distance' = to_tune(1, 5),
    'kknn.kernel' = to_tune(levels = c("rectangular", "optimal", "epanechnikov", "biweight",
                                       "triweight", "cos", "inv", "gaussian", "rank"))
  ),
  'ranger' = list(
    'ranger.mtry.ratio' = to_tune(lower = 0, upper = 1),
    'ranger.replace' = to_tune(levels = c(TRUE, FALSE)),
    'ranger.sample.fraction' = to_tune(0.1, 1)
  ),
  'rpart' = list(
    'rpart.minsplit' = to_tune(2, 128, logscale = TRUE),
    'rpart.minbucket' = to_tune(1, 64, logscale = TRUE),
    'rpart.cp' = to_tune(1e-04, 0.1, logscale = TRUE)
  ),
  'xgboost' = list(
    'xgboost.eta' = to_tune(1e-04, 1, logscale = TRUE),
    'xgboost.max_depth' = to_tune(1, 20),
    'xgboost.colsample_bytree' = to_tune(1e-01, 1),
    'xgboost.colsample_bylevel' = to_tune(1e-01, 1),
    'xgboost.lambda' = to_tune(1e-03, 1000, logscale = TRUE),
    'xgboost.alpha' = to_tune(1e-03, 1000, logscale = TRUE),
    'xgboost.subsample' = to_tune(1e-01, 1)
  )
)


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
