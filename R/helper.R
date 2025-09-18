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

sample_design_set = function(task, measure, size, learner_id, search_space, exclude = NULL, stratify = TRUE) {
  assert_task(task)
  assert_measure(measure)
  assert_count(size)
  assert_data_table(exclude, null.ok = TRUE)

  # read data of best hyperparameters
  data = fread(system.file("data", sprintf("best_%s.csv", learner_id), package = "mlr3automl"))

  # exclude tasks
  exclude_tasks = getOption("mlr3automl.exclude_tasks", "")
  if (any(exclude_tasks %in% data$task)) {
    lg$info("Excluding tasks from initial design: %s", as_short_string(exclude_tasks[exclude_tasks %in% data$task]))
    data = data[task %nin% exclude_tasks]
  }

  # subset to relevant measure
  measure_id = sub(sprintf("^%s\\.", task$task_type), "", measure$id)
  measure_id = if (measure_id %in% data$measure) measure_id else "mcc"
  data = data[measure_id, , on = "measure"]

  # subset to relevant parameters
  data = set_names(data, sprintf("%s.%s", learner_id, names(data)))
  param_ids = search_space$ids()
  internal_ids = search_space$ids(tags = "internal_tuning")
  param_ids = setdiff(param_ids, internal_ids)
  data = data[, param_ids, with = FALSE]

  # exclude already sampled rows
  if (!is.null(exclude)) {
    data = data[!exclude, , on = param_ids]
  }

  # draw at least one row for each factor level
  factor_cols = search_space$ids(class = c("ParamFct", "ParamLgl"))
  if (stratify && length(factor_cols)) {

    # sample rows for each factor level
    xdt = map_dtr(factor_cols, function(col) {
      data[, .SD[sample(.N, 1)], by = col]
    })

    # sample remaining rows until size is reached
    data = data[!xdt, , on = param_ids]
    size = size - nrow(xdt)
    if (size > 0) {
      size = min(size, nrow(data))
      xdt = rbindlist(list(xdt, data[sample(nrow(data), size)]), use.names = TRUE)
    }
  } else {
    size = min(size, nrow(data))
    xdt = data[sample(nrow(data), size)]
  }

  lg$info("Learner '%s' design set size: %i", learner_id, nrow(xdt))

  set(xdt, j = "branch.selection", value = learner_id)
  xdt
}
