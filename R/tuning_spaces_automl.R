#' @title AutoML Tuning Spaces
#'
#' @name mlr_tuning_spaces_automl
#'
#' @description
#' Tuning spaces from the mlr3automl package.
#'
#' @aliases
#' mlr_tuning_spaces_classif.glmnet.automl
#' mlr_tuning_spaces_classif.kknn.automl
#' mlr_tuning_spaces_classif.ranger.automl
#' mlr_tuning_spaces_classif.rpart.automl
#' mlr_tuning_spaces_classif.svm.automl
#' mlr_tuning_spaces_classif.xgboost.automl
#' mlr_tuning_spaces_regr.glmnet.automl
#' mlr_tuning_spaces_regr.kknn.automl
#' mlr_tuning_spaces_regr.ranger.automl
#' mlr_tuning_spaces_regr.rpart.automl
#' mlr_tuning_spaces_regr.svm.automl
#' mlr_tuning_spaces_regr.xgboost.automl
#'
#' @section glmnet tuning space:
#' `r rd_info(lts("classif.glmnet.automl"))`
#'
#' @section kknn tuning space:
#' `r rd_info(lts("classif.kknn.automl"))`
#'
#' @section ranger tuning space:
#' `r rd_info(lts("classif.ranger.automl"))`
#'
#' @section rpart tuning space:
#' `r rd_info(lts("classif.rpart.automl"))`
#'
#' @section svm tuning space:
#' `r rd_info(lts("classif.svm.automl"))`
#'
#' @section xgboost tuning space:
#' `r rd_info(lts("classif.xgboost.automl"))`
NULL

add_tuning_space = function(id, values, tags, learner, package = character(), label = NA_character_, man = NA_character_) { # nolint
  tuning_space = TuningSpace$new(id, values, tags, learner, package, label = label, man = paste0("mlr3automl:", id))
  mlr_tuning_spaces$add(id, tuning_space)
}

# glmnet
vals = list(
  s     = to_tune(1e-4, 1e4, logscale = TRUE),
  alpha = to_tune(0, 1)
)

add_tuning_space(
  id = "classif.glmnet.automl",
  values = vals,
  tags = c("default", "classification"),
  learner = "classif.glmnet",
  package = "mlr3learners",
  label = "Classification GLM with Default"
)

add_tuning_space(
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

add_tuning_space(
  id = "classif.kknn.automl",
  values = vals,
  tags = c("default", "classification"),
  learner = "classif.kknn",
  package = "mlr3learners",
  label = "Classification KKNN with Default"
)

add_tuning_space(
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

add_tuning_space(
  id = "classif.ranger.automl",
  values = vals,
  tags = c("default", "classification"),
  learner = "classif.ranger",
  label = "Classification Ranger with Default"
)

add_tuning_space(
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

add_tuning_space(
  id = "classif.rpart.automl",
  values = vals,
  tags = c("default", "classification"),
  learner = "classif.rpart",
  label = "Classification Rpart with Default"
)

add_tuning_space(
  id = "regr.rpart.automl",
  values = vals,
  tags = c("default", "regression"),
  learner = "regr.rpart",
  label = "Regression Rpart with Default"
)

# svm
vals = list(
  cost    = to_tune(1e-4, 1e4, logscale = TRUE),
  kernel  = to_tune(c("polynomial", "radial", "sigmoid", "linear")),
  degree  = to_tune(2, 5),
  gamma   = to_tune(1e-4, 1e4, logscale = TRUE)
)

add_tuning_space(
  id = "classif.svm.automl",
  values = vals,
  tags = c("default", "classification"),
  learner = "classif.svm",
  package = "mlr3learners",
  label = "Classification SVM with Default"
)

add_tuning_space(
  id = "regr.svm.automl",
  values = vals,
  tags = c("default", "regression"),
  learner = "regr.svm",
  package = "mlr3learners",
  label = "Regression SVM with Default"
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

add_tuning_space(
  id = "classif.xgboost.automl",
  values = vals,
  tags = c("default", "classification"),
  learner = "classif.xgboost",
  package = "mlr3learners",
  label = "Classification XGBoost with Default"
)

add_tuning_space(
  id = "regr.xgboost.automl",
  values = vals,
  tags = c("default", "regression"),
  learner = "regr.xgboost",
  package = "mlr3learners",
  label = "Regression XGBoost with Default"
)
