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
