#' @title Initial Design Runtime Limit Callback
#'
#' @name mlr3automl.initial_design_runtime
#'
#' @description
#' This [mlr3misc::Callback] fails the tasks of the initial design if 25% of the runtime limit is reached.
#'
#' @examples
#' clbk("mlr3automl.initial_design_runtime")
NULL

load_callback_initial_design_runtime = function() {
  callback_batch_tuning("mlr3automl.initial_design_runtime",
    label = "Initial Design Runtime Limit Callback",
    man = "mlr3automl::mlr3automl.initial_design_runtime",

    on_optimizer_after_eval = function(callback, context) {
      start_time = context$instance$archive$start_time
      runtime_limit = context$instance$terminator$param_set$values$secs
      if (difftime(Sys.time(), start_time, units = "secs") > runtime_limit * 0.25) {
        lg = lgr::get_logger("mlr3/mlr3automl")
        lg$info("Initial design runtime limit reached")
        failed_tasks = context$instance$rush$queued_tasks
        if (length(failed_tasks)) {
          context$instance$rush$push_failed(failed_tasks, conditions = replicate(length(failed_tasks), list(message = "Initial design runtime limit reached"), simplify = FALSE))
        }
      }
    }
  )
}

callbacks[["mlr3automl.initial_design_runtime"]] = load_callback_initial_design_runtime
