xgboost_internal_measure = function(measure, task) {
  id = measure$id

  metric = if (task$task_type == "regr") {
    switch(id,
      "regr.rmse" = "rmse",
      "regr.rmsle" = "rmsle",
      "regr.mae" = "mae",
      "regr.mape" = "mape",
      "regr.logloss" = "logloss",
      NULL
    )
  } else if ("twoclass" %in% task$properties) {
    switch(id,
      "classif.ce" = "error",
      "classif.acc" = "error",
      "classif.auc" = "auc",
      NULL
    )
  } else if ("multiclass" %in% task$properties) {
    switch(id,
      "classif.ce" = "merror",
      "classif.acc" = "merror",
      NULL
    )
  }

  return(metric %??% NA_character_)
}

catboost_internal_measure = function(measure, task) {
  id = measure$id

  metric = if (task$task_type == "regr") {
    switch(id,
      "regr.rmse" = "RMSE",
      "regr.rmsle" = "RMSLE",
      "regr.mae" = "MAE",
      "regr.mape" = "MAPE",
      "regr.logloss" = "Logloss",
      NULL
    )
  } else if ("twoclass" %in% task$properties) {
    switch(id,
      "classif.ce" = "Accuracy",
      "classif.acc" = "Accuracy",
      "classif.auc" = "AUC",
      NULL
    )
  } else if ("multiclass" %in% task$properties) {
    switch(id,
      "classif.ce" = "Accuracy",
      "classif.acc" = "Accuracy",
      "classif.auc" = "AUC",
      "classif.logloss" = "MultiLogloss",
      NULL
    )
  }

  return(metric %??% NA_character_)
}

lightgbm_internal_measure = function(measure, task) {
  id = measure$id

  metric = if (task$task_type == "regr") {
    switch(id,
      "regr.rmse" = "rmse",
      "regr.mae" = "mae",
      "regr.mape" = "mape",
      "regr.logloss" = "logloss",
      NULL
    )
  } else if ("twoclass" %in% task$properties) {
    switch(id,
      "classif.ce" = "error",
      "classif.acc" = "error",
      "classif.auc" = "auc",
      NULL
    )
  } else if ("multiclass" %in% task$properties) {
    switch(id,
      "classif.ce" = "merror",
      "classif.acc" = "merror",
      "classif.auc" = "auc_mu",
      NULL
    )
  }

  return(metric %??% NA_character_)
}
