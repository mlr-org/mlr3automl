load_callback_initial_design = function() {
  callback_tuning("mlr3automl.initial_design",
    label = "Initial Design Callback",
    on_optimization_begin = function(callback, context) {
      assert_data_table(callback$state$design)
      context$instance$eval_batch(callback$state$design)
    }
  )
}

load_callback_nrounds = function() {
  callback_tuning("mlr3automl.nrounds",
    label = "Boosting Rounds Callback",

    on_eval_after_design = function(callback, context) {
      context$objective_tuning$store_models = TRUE
    },

    on_eval_after_benchmark = function(callback, context) {
      callback$state$max_nrounds = map_dbl(context$benchmark_result$resample_results$resample_result, function(rr) {
          max(map_dbl(get_private(rr)$.data$learner_states(get_private(rr)$.view), function(state) {
            if (inherits(state$model$xgboost, "NO_OP") || is.null(state$model$xgboost$model$best_iteration)) {
              NA_real_
            } else {
              state$model$xgboost$model$best_iteration
            }
          }))
      })
    },

    on_eval_before_archive = function(callback, context) {
      set(context$aggregated_performance, j = "max_nrounds", value = callback$state$max_nrounds)
      context$benchmark_result$discard(models = TRUE)
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
