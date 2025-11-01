cb_timeout_xgboost = function(timeout) {
  callback = function(env = parent.frame()) {
    if (is.null(env$start_time)) {
      env$start_time = Sys.time()
    }

    if (difftime(Sys.time(), env$start_time, units = "secs") > timeout) {
      message("Timeout reached")
      env$stop_condition = TRUE
    } else {
      env$stop_condition = FALSE
    }
  }
  attr(callback, 'call') = match.call()
  attr(callback, 'name') = 'cb_timeout_xgboost'
  callback
}


cb_timeout_lightgbm = function(timeout) {

  state = new.env(parent = emptyenv())

  callback = function(env) {
    if (is.null(state$start_time)) state$start_time = Sys.time()

    if (difftime(Sys.time(), state$start_time, units = "secs") > timeout) {
      message("Timeout reached")
      env$met_early_stop = TRUE
    } else {
      env$met_early_stop = FALSE
    }
  }

  attr(callback, "call") = match.call()
  attr(callback, "name") = "cb_timeout_lightgbm"

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

combine_search_spaces = function(autos, task) {
  learner_ids = map_chr(autos, "id")

  # initialize search space
  search_space = ps(
    branch.selection = p_fct(levels = learner_ids)
  )

  search_spaces = map(autos, function(auto) auto$search_space(task))
  union_space = ps_union(c(list(search_space), unname(search_spaces)))

  # add dependencies
  iwalk(search_spaces, function(search_space, learner_id) {
    param_ids = search_space$ids()
    internal_tune_ids = search_space$ids(any_tags = "internal_tuning")
    param_ids = setdiff(param_ids, internal_tune_ids)

    walk(param_ids, function(param_id) {
      union_space$add_dep(
        id = param_id,
        on = "branch.selection",
        cond = CondEqual$new(learner_id)
      )
    })
  })

  union_space
}

generate_initial_design = function(method, search_space, size) {
  internal_tune_ids = search_space$ids(any_tags = "internal_tuning")

  xdt = switch(method,
    "sobol" = generate_design_sobol(search_space, size)$data,
    "lhs" = generate_design_lhs(search_space, size)$data,
    "random" = generate_design_random(search_space, size)$data,
  )

  xdt[, setdiff(c("branch.selection", search_space$ids()), internal_tune_ids), with = FALSE]
}

