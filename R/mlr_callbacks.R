
load_callback_nrounds = function() {
  callback_rush_tuning("mlr3automl.nrounds",
    label = "Boosting Rounds Callback",

    on_eval_after_xs = function(callback, context) {
      context$objective_tuning$store_models = TRUE
    },

    on_eval_after_resample = function(callback, context) {
      states = get_private(context$resample_result)$.data$learner_states(get_private(context$resample_result)$.view)

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
      if (context$result$learner_param_vals[[1]]$branch.selection == "xgboost") {
        context$result$learner_param_vals[[1]]$xgboost.early_stopping_rounds = NULL
        context$result$learner_param_vals[[1]]$xgboost.callbacks = list(cb.timeout(timeout = Inf))
        context$result$learner_param_vals[[1]]$xgboost.nrounds = context$instance$archive$best()$max_nrounds
        context$result$learner_param_vals[[1]]$xgboost.holdout_task = NULL
      }
    }
  )
}
