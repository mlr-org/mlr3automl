
load_callback_timeout = function() {
  callback_tuning("mlr3tuning.timeout",
    label = "Timeout Callback",
    on_optimization_begin = function(callback, context) {
      assert_count(callback$state$time_limit)
      assert_count(callback$state$max_time_limit, null.ok = TRUE)
      callback$state$start_time = Sys.time()
    },

    on_eval_after_design = function(callback, context) {
      remaining_time = max(0, callback$state$time_limit - as.integer(difftime(Sys.time(), callback$state$start_time, units = "secs")))
      remaining_time = min(remaining_time, callback$state$max_time_limit)

      map(context$design$learner, function(learner) {
        learner$timeout = c(train = remaining_time, predict = Inf)
      })
    },
  )
}

load_callback_initial_design = function() {
  callback_tuning("mlr3tuning.initial_design",
    label = "Initial Design Callback",
    on_optimization_begin = function(callback, context) {
      assert_data_table(callback$state$design)
      context$instance$eval_batch(callback$state$design)
    }
  )
}
