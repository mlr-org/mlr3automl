#' @title Initial Design Runtime Limit Callback
#'
#' @name mlr3automl.initial_design_runtime
#'
#' @description
#' This [mlr3misc::Callback] drops the remaining tasks of the initial design from the queue when a
#' configurable fraction (`initial_design_fraction`) of the runtime limit is reached.
#' The `initial_design_fraction` is set to `0.25` by default.
#'
#' @examples
#' clbk("mlr3automl.initial_design_runtime", initial_design_fraction = 0.5)
NULL

# nolint next: object_length_linter
load_callback_initial_design_runtime = function() {
  callback_async_tuning(
    "mlr3automl.initial_design_runtime",
    label = "Initial Design Runtime Limit Callback",
    man = "mlr3automl::mlr3automl.initial_design_runtime",
    on_optimizer_queue_after_eval = function(callback, context) {
      if (inherits(context$instance$terminator, "TerminatorRunTime")) {
        initial_design_fraction = callback$state$initial_design_fraction %??% 0.25
        start_time = context$instance$archive$start_time
        runtime_limit = context$instance$terminator$param_set$values$secs

        if (difftime(Sys.time(), start_time, units = "secs") > runtime_limit * initial_design_fraction) {
          lg = lgr::get_logger("mlr3/mlr3automl")
          lg$info("Initial design runtime limit reached")
          context$instance$rush$empty_queue()
        }
      }
    }
  )
}

callbacks[["mlr3automl.initial_design_runtime"]] = load_callback_initial_design_runtime

#' @title Encapsulation Daemon Callback
#'
#' @name mlr3automl.encapsulation_daemon
#'
#' @description
#' This [mlr3misc::Callback] starts a persistent \CRANpkg{mirai} daemon on each rush worker that is reused for the
#' `"mirai"` encapsulation of the tuned learners.
#' Reusing a single daemon avoids the overhead of starting a new daemon for every learner evaluation.
#' The daemon is started when the worker begins and its liveness is checked before every evaluation.
#' If the daemon has died, it is restarted.
#'
#' @section Parameters:
#' * `n_daemons` :: `integer(1)`\cr
#'   Number of daemons to start on each worker.
#'   Defaults to `1`.
#'
#' @examples
#' clbk("mlr3automl.encapsulation_daemon", n_daemons = 1)
NULL

# nolint next: object_length_linter
load_callback_encapsulation_daemon = function() {
  ensure_daemon = function(n) {
    compute = getOption("mlr3.mirai_encapsulation", "mlr3_encapsulation")
    if (mirai::status(.compute = compute)$connections < n) {
      lgr::get_logger("mlr3/mlr3automl")$debug(
        "Starting %i encapsulation daemon(s) on compute profile '%s'",
        n,
        compute
      )
      # reset first to clear any stale daemon state before (re)starting
      mirai::daemons(0L, .compute = compute)
      mirai::daemons(n, .compute = compute)
    }
  }

  callback_async_tuning(
    "mlr3automl.encapsulation_daemon",
    label = "Encapsulation Daemon Callback",
    man = "mlr3automl::mlr3automl.encapsulation_daemon",
    on_worker_begin = function(callback, context) {
      ensure_daemon(callback$state$n_daemons %??% 1L)
    },
    on_optimizer_before_eval = function(callback, context) {
      ensure_daemon(callback$state$n_daemons %??% 1L)
    },
    on_worker_end = function(callback, context) {
      mirai::daemons(0L, .compute = getOption("mlr3.mirai_encapsulation", "mlr3_encapsulation"))
    }
  )
}

callbacks[["mlr3automl.encapsulation_daemon"]] = load_callback_encapsulation_daemon
