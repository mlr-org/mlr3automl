add_tuning_space = function(id, values, tags, learner, package = character(), label = NA_character_, man = NA_character_) { # nolint
  TuningSpace$new(id, values, tags, learner, package, label = label, man = paste0("mlr3automl:", id))
}

# glmnet
vals = list(
  s     = to_tune(1e-4, 1e4, logscale = TRUE),
  alpha = to_tune(0, 1)
)

tuning_spaces[["classif.glmnet.automl"]] = add_tuning_space(
  id = "classif.glmnet.automl",
  values = vals,
  tags = c("default", "classification"),
  learner = "classif.glmnet",
  package = "mlr3learners",
  label = "Classification GLM with Default"
)

tuning_spaces[["regr.glmnet.automl"]] = add_tuning_space(
  id = "regr.glmnet.automl",
  values = vals,
  tags = c("default", "regression"),
  learner = "regr.glmnet",
  package = "mlr3learners",
  label = "Regression GLM with Default"
)

# kknn
vals = list(
  k = to_tune(1, 50, logscale = TRUE),
  distance = to_tune(1, 5),
  kernel = to_tune(c("rectangular", "optimal", "epanechnikov", "biweight", "triweight", "cos",  "inv",  "gaussian", "rank"))
)

tuning_spaces[["classif.kknn.automl"]] = add_tuning_space(
  id = "classif.kknn.automl",
  values = vals,
  tags = c("default", "classification"),
  learner = "classif.kknn",
  package = "mlr3learners",
  label = "Classification KKNN with Default"
)

tuning_spaces[["regr.kknn.automl"]] = add_tuning_space(
  id = "regr.kknn.automl",
  values = vals,
  tags = c("default", "regression"),
  learner = "regr.kknn",
  package = "mlr3learners",
  label = "Regression KKNN with Default"
)

# ranger
vals = list(
  mtry.ratio      = to_tune(0, 1),
  replace         = to_tune(p_lgl()),
  sample.fraction = to_tune(1e-1, 1)
)

tuning_spaces[["classif.ranger.automl"]] = add_tuning_space(
  id = "classif.ranger.automl",
  values = vals,
  tags = c("default", "classification"),
  learner = "classif.ranger",
  label = "Classification Ranger with Default"
)

tuning_spaces[["regr.ranger.automl"]] = add_tuning_space(
  id = "regr.ranger.automl",
  values = vals,
  tags = c("default", "regression"),
  learner = "regr.ranger",
  label = "Regression Ranger with Default"
)

# rpart
vals = list(
  minsplit  = to_tune(2, 128, logscale = TRUE),
  minbucket = to_tune(1, 64, logscale = TRUE),
  cp        = to_tune(1e-04, 1e-1, logscale = TRUE)
)

tuning_spaces[["classif.rpart.automl"]] = add_tuning_space(
  id = "classif.rpart.automl",
  values = vals,
  tags = c("default", "classification"),
  learner = "classif.rpart",
  label = "Classification Rpart with Default"
)

tuning_spaces[["regr.rpart.automl"]] = add_tuning_space(
  id = "regr.rpart.automl",
  values = vals,
  tags = c("default", "regression"),
  learner = "regr.rpart",
  label = "Regression Rpart with Default"
)

# xgboost
vals = list(
  eta               = to_tune(1e-4, 1, logscale = TRUE),
  max_depth         = to_tune(1, 20),
  colsample_bytree  = to_tune(1e-1, 1),
  colsample_bylevel = to_tune(1e-1, 1),
  lambda            = to_tune(1e-3, 1e3, logscale = TRUE),
  alpha             = to_tune(1e-3, 1e3, logscale = TRUE),
  subsample         = to_tune(1e-1, 1)
)

tuning_spaces[["classif.xgboost.automl"]] = add_tuning_space(
  id = "classif.xgboost.automl",
  values = vals,
  tags = c("default", "classification"),
  learner = "classif.xgboost",
  package = "mlr3learners",
  label = "Classification XGBoost with Default"
)

tuning_spaces[["regr.xgboost.automl"]] = add_tuning_space(
  id = "regr.xgboost.automl",
  values = vals,
  tags = c("default", "regression"),
  learner = "regr.xgboost",
  package = "mlr3learners",
  label = "Regression XGBoost with Default"
)
