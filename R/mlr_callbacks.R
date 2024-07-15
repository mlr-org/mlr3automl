
load_callback_branch_nrounds = function() {
  callback_async_tuning("mlr3automl.branch_nrounds",
    label = "Boosting Rounds Callback",

    on_eval_after_xs = function(callback, context) {
      context$instance$objective$store_models = TRUE
    },

    on_eval_after_resample = function(callback, context) {
      states = mlr3misc::get_private(context$resample_result)$.data$learner_states(mlr3misc::get_private(context$resample_result)$.view)

      callback$state$max_nrounds = max(map_dbl(states, function(state) {
        if (!inherits(state$model$xgboost, "NO_OP")) {
          state$model$xgboost$model$best_iteration %??% NA_real_
        } else if (!inherits(state$model$catboost, "NO_OP")) {
          state$model$catboost$model$tree_count %??% NA_real_
        } else if (!inherits(state$model$lightgbm, "NO_OP")) {
          state$model$lightgbm$model$best_iter %??% NA_real_
        } else {
          NA_real_
        }
      }), na.rm = TRUE)
    },

    on_eval_before_archive = function(callback, context) {
      context$aggregated_performance = c(context$aggregated_performance, list(max_nrounds = callback$state$max_nrounds))
      context$resample_result$discard(models = TRUE)
    },

    on_result = function(callback, context) {
      if (context$result$learner_param_vals[[1]]$branch.selection == "xgboost") {
        context$result$learner_param_vals[[1]]$xgboost.early_stopping_rounds = NULL
        context$result$learner_param_vals[[1]]$xgboost.callbacks = list(cb_timeout_xgboost(timeout = Inf))
        context$result$learner_param_vals[[1]]$xgboost.nrounds = max(context$instance$archive$best()$max_nrounds, 1)
        context$result$learner_param_vals[[1]]$xgboost.holdout_task = NULL
      } else if (context$result$learner_param_vals[[1]]$branch.selection == "catboost") {
        context$result$learner_param_vals[[1]]$catboost.early_stopping_rounds = NULL
        context$result$learner_param_vals[[1]]$catboost.iterations = max(context$instance$archive$best()$max_nrounds, 1)
        context$result$learner_param_vals[[1]]$catboost.holdout_task = NULL
        context$result$learner_param_vals[[1]]$catboost.eval_metric = NULL
      } else if (context$result$learner_param_vals[[1]]$branch.selection == "lightgbm") {
        context$result$learner_param_vals[[1]]$lightgbm.early_stopping_rounds = NULL
        context$result$learner_param_vals[[1]]$lightgbm.num_iterations = max(context$instance$archive$best()$max_nrounds, 1)
        context$result$learner_param_vals[[1]]$lightgbm.holdout_task = NULL
        context$result$learner_param_vals[[1]]$lightgbm.callbacks = NULL
      }
    }
  )
}

load_callback_nrounds = function() {
  callback_async_tuning("mlr3automl.nrounds",
    label = "Boosting Rounds Callback",

    on_eval_after_xs = function(callback, context) {
      context$objective_tuning$store_models = TRUE
    },

    on_eval_after_resample = function(callback, context) {
      states = mlr3misc::get_private(context$resample_result)$.data$learner_states(mlr3misc::get_private(context$resample_result)$.view)

      callback$state$max_nrounds = max(map_dbl(states, function(state) {
        if (inherits(state$model$xgboost, "NO_OP") || is.null(state$model$xgboost$model$best_iteration)) {
          NA_real_
        } else {
          state$model$xgboost$model$best_iteration
        }
      }))
    },

    on_eval_before_archive = function(callback, context) {
      context$aggregated_performance = c(context$aggregated_performance, list(max_nrounds = callback$state$max_nrounds))
      context$resample_result$discard(models = TRUE)
    },

    on_result = function(callback, context) {
      context$result$learner_param_vals[[1]]$xgboost.early_stopping_rounds = NULL
      context$result$learner_param_vals[[1]]$xgboost.callbacks = list(cb_timeout_xgboost(timeout = Inf))
      context$result$learner_param_vals[[1]]$xgboost.nrounds = context$instance$archive$best()$max_nrounds
      context$result$learner_param_vals[[1]]$xgboost.holdout_task = NULL
    }
  )
}
