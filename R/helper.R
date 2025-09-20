cb_timeout_xgboost = function(timeout) {
  callback = function(env = parent.frame()) {
    if (is.null(env$start_time)) {
      env$start_time <- Sys.time()
    }

    if (difftime(Sys.time(), env$start_time, units = "secs") > timeout) {
      message("Timeout reached")
      env$stop_condition <- TRUE
    } else {
      env$stop_condition <- FALSE
    }
  }
  attr(callback, 'call') = match.call()
  attr(callback, 'name') = 'cb_timeout_xgboost'
  callback
}


cb_timeout_lightgbm <- function(timeout) {

  state <- new.env(parent = emptyenv())

  callback = function(env) {
    if (is.null(state$start_time)) state$start_time <- Sys.time()

    if (difftime(Sys.time(), state$start_time, units = "secs") > timeout) {
      message("Timeout reached")
      env$met_early_stop = TRUE
    } else {
      env$met_early_stop = FALSE
    }
  }

  attr(callback, "call") <- match.call()
  attr(callback, "name") <- "cb_timeout_lightgbm"

  return(callback)
}

check_python_packages = function(packages, python_version = NULL) {
  reticulate::py_require(packages, python_version = python_version)
  available = map_lgl(packages, reticulate::py_module_available)
  if (any(!available)) {
    return(sprintf("Package %s not available.", as_short_string(packages[!available])))
  }
  TRUE
}

assert_python_packages = function(packages, python_version = NULL) {
  reticulate::py_require(packages, python_version = python_version)
  available = map_lgl(packages, reticulate::py_module_available)
  if (any(!available)) {
    stopf("Package %s not available.", as_short_string(packages[!available]))
  }
  invisible(packages)
}

callback_runtime_limit = callback_async_tuning("initial_design_runtime",
  on_optimizer_after_eval = function(callback, context) {
    start_time = context$instance$archive$start_time
    runtime_limit = context$instance$terminator$param_set$values$secs
    if (difftime(Sys.time(), start_time, units = "secs") > runtime_limit * 0.25) {
      lg$info("Initial design runtime limit reached")
      failed_tasks = context$instance$rush$queued_tasks
      if (length(failed_tasks)) {
        context$instance$rush$push_failed(failed_tasks, condition = replicate(length(failed_tasks), list(message = "Initial design runtime limit reached"), simplify = FALSE))
      }
    }
  }
)
