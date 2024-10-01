#' @title Internal Measure Catboost
#'
#' @description
#' Function to get the internal catboost measure for a given [mlr3::Task] and [mlr3::Measure].
#' For example, the measure "classif.auc" will return "AUC" for a binary classification task.
#'
#' @param measure [mlr3::Measure]\cr
#' Measure to get the internal measure for.
#' @param task [mlr3::Task]\cr
#' Task to get the internal measure for.
#'
#' @export
#' @examples
#' internal_measure_catboost(msr("classif.auc"), tsk("pima"))
internal_measure_catboost = function(measure, task) {
  id = measure$id

  metric = if (task$task_type == "regr") {
    switch(id,
      "regr.rmse" = "RMSE",
      "regr.mae" = "MAE",
      "regr.mape" = "MAPE",
      "regr.smape" = "SMAPE",
      "regr.medae" = "MedianAbsoluteError",
      "rsq" = "R2",  # regr.rsq has id `rsq`
      NULL
    )
  } else if ("twoclass" %in% task$properties) {
    switch(id,
      "classif.ce" = "Accuracy",
      "classif.acc" = "Accuracy",
      "classif.bacc" = "BalancedAccuracy",
      "classif.auc" = "AUC",
      "classif.prauc" = "PRAUC",
      "classif.bbrier" = "BrierScore",
      "classif.logloss" = "Logloss",
      "classif.precision" = "Precision",
      "classif.recall" = "Recall",
      "classif.mcc" = "MCC",
      NULL
    )
  } else if ("multiclass" %in% task$properties) {
    switch(id,
      "classif.ce" = "Accuracy",
      "classif.acc" = "Accuracy",
      "classif.mauc_mu" = "AUC",
      "classif.logloss" = "MultiClass",
      "classif.mcc" = "MCC",
      NULL
    )
  }

  return(metric %??% NA_character_)
}

#' @title Internal Measure LightGBM
#'
#' @description
#' Function to get the internal lightgbm measure for a given [mlr3::Task] and [mlr3::Measure].
#' For example, the measure "classif.auc" will return "auc" for a binary classification task.
#'
#' @param measure [mlr3::Measure]\cr
#' Measure to get the internal measure for.
#' @param task [mlr3::Task]\cr
#' Task to get the internal measure for.
#'
#' @export
#' @examples
#' internal_measure_lightgbm(msr("classif.auc"), tsk("pima"))
internal_measure_lightgbm = function(measure, task) {
  id = measure$id

  metric = if (task$task_type == "regr") {
    switch(id,
      "regr.mse" = "mse",
      "regr.rmse" = "rmse",
      "regr.mae" = "mae",
      "regr.mape" = "mape",
      NULL
    )
  } else if ("twoclass" %in% task$properties) {
    switch(id,
      "classif.ce" = "binary_error",
      "classif.acc" = "binary_error",
      "classif.auc" = "auc",
      "classif.logloss" = "binary_logloss",
      NULL
    )
  } else if ("multiclass" %in% task$properties) {
    switch(id,
      "classif.ce" = "multi_error",
      "classif.acc" = "multi_error",
      "classif.mauc_mu" = "auc_mu",
      "classif.logloss" = "multi_logloss",
      NULL
    )
  }

  return(metric %??% NA_character_)
}


#' @title Internal Measure XGBoost
#'
#' @description
#' Function to get the internal xgboost measure for a given [mlr3::Task] and [mlr3::Measure].
#' For example, the measure "classif.auc" will return "auc" for a binary classification task.
#'
#' @param measure [mlr3::Measure]\cr
#' Measure to get the internal measure for.
#' @param task [mlr3::Task]\cr
#' Task to get the internal measure for.
#'
#' @export
#' @examples
#' internal_measure_xgboost(msr("classif.auc"), tsk("pima"))
internal_measure_xgboost = function(measure, task) {
  id = measure$id

  metric = if (task$task_type == "regr") {
    switch(id,
      "regr.mse" = "mse",
      "regr.rmse" = "rmse",
      "regr.mae" = "mae",
      "regr.mape" = "mape",
      NULL
    )
  } else if ("twoclass" %in% task$properties) {
    switch(id,
      "classif.ce" = "error",
      "classif.acc" = "error",
      "classif.auc" = "auc",
      "classif.prauc" = "aucpr",
      "classif.logloss" = "logloss",
      NULL
    )
  } else if ("multiclass" %in% task$properties) {
    switch(id,
      "classif.ce" = "merror",
      "classif.acc" = "merror",
      "classif.mauc_aunp" = "auc",
      "classif.logloss" = "mlogloss",
      NULL
    )
  }

  return(metric %??% NA_character_)
}
