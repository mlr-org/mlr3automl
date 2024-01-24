
load_callback_timeout = function() {
  callback_tuning("mlr3tuning.timeout",
    label = "Timeout Callback",
    on_optimization_begin = function(callback, context) {
      assert_count(callback$state$run_time)
      assert_count(callback$state$timeout, null.ok = TRUE)
      callback$state$start_time = Sys.time()
    },

    on_eval_after_design = function(callback, context) {
      iterations = context$objective_tuning$resampling$iters * max(nrow(context$design), length(context$design$param_values[[1]]))
      remaining_time = callback$state$run_time - difftime(Sys.time(), callback$state$start_time, units = "secs")
      timeout = max(0, remaining_time / iterations)
      # limit to learner timeout
      timeout = min(timeout, callback$state$timeout)

      print(sprintf("Timeout: %s", timeout))

      map(context$design$learner, function(learner) {
        learner$timeout = c(train = timeout, predict = timeout)
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
