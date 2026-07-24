cb_timeout_xgboost = function(timeout) {
  cb_env = new.env(parent = emptyenv())
  xgboost::xgb.Callback(
    cb_name = "cb_timeout_xgboost",
    env = cb_env,
    # the callback environment is created once at graph construction and shared
    # across trainings, so the clock must be reset at the start of each training
    f_before_training = function(env, model, data, evals, begin_iteration, end_iteration) {
      env$start_time = Sys.time()
    },
    f_after_iter = function(env, model, data, evals, iteration, iter_feval) {
      if (difftime(Sys.time(), env$start_time, units = "secs") > timeout) {
        message("Timeout reached")
        TRUE
      } else {
        FALSE
      }
    }
  )
}


cb_timeout_lightgbm = function(timeout) {
  # nolint next: object_usage_linter
  state = new.env(parent = emptyenv())

  callback = function(env) {
    # the state environment is created once at graph construction and shared
    # across trainings, so the clock must be reset at the start of each training
    if (env$iteration == env$begin_iteration) {
      state$start_time = Sys.time()
    }

    if (difftime(Sys.time(), state$start_time, units = "secs") > timeout) {
      message("Timeout reached")
      env$met_early_stop = TRUE
    } else {
      env$met_early_stop = FALSE
    }
  }

  attr(callback, "call") = match.call()
  attr(callback, "name") = "cb_timeout_lightgbm"

  callback
}

# initializing python exports VIRTUAL_ENV, PYTHONPATH and
# R_SESSION_INITIALIZED, which child processes inherit. reticulate in a child
# would bind to the parent python environment instead of resolving the
# requirements declared with py_require(). call before py_require() in the
# callr sessions used for process isolation.
clean_reticulate_env = function() {
  Sys.unsetenv(c("VIRTUAL_ENV", "VIRTUAL_ENV_PROMPT", "PYTHONPATH", "R_SESSION_INITIALIZED"))
}

# memoizes check_python_packages() results so repeated probes for the same
# requirements in one session do not spawn a callr session each time.
python_package_cache = new.env(parent = emptyenv())

check_python_packages = function(packages, python_version = NULL) {
  # the probe is expensive (a callr session that resolves the python environment)
  # and the result is stable within a session, so cache it per requirement set.
  key = calculate_hash(packages, python_version)
  cached = python_package_cache[[key]]
  if (!is.null(cached)) {
    return(cached)
  }

  # py_module_available() initializes python and imports the modules into the
  # calling session. probe in a short-lived callr session instead, so python
  # torch is never loaded into a process that also uses the torch package
  # (mlr3torch). the libtorch versions are incompatible and whichever stack
  # loads second breaks.
  available = tryCatch(
    callr::r(
      function(packages, python_version) {
        Sys.unsetenv(c("VIRTUAL_ENV", "VIRTUAL_ENV_PROMPT", "PYTHONPATH", "R_SESSION_INITIALIZED"))
        reticulate::py_require(packages, python_version = python_version)
        # strip version specifiers e.g. "fastcore<2.0.0" -> "fastcore"
        modules = gsub("[<>=!~].*$", "", packages)
        vapply(modules, reticulate::py_module_available, logical(1L), USE.NAMES = FALSE)
      },
      args = list(packages = packages, python_version = python_version)
    ),
    error = function(e) sprintf("Python package check failed: %s", conditionMessage(e))
  )
  result = if (is.character(available)) {
    available
  } else if (any(!available)) {
    sprintf("Package %s not available.", as_short_string(packages[!available]))
  } else {
    TRUE
  }
  python_package_cache[[key]] = result
  result
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

# choose the most informative predict type that satisfies every requested type.
# a "prob" learner also produces "response", so the graph must predict "prob"
# when either the measure or the user asks for it.
highest_predict_type = function(task_type, predict_types) {
  ordering = mlr_reflections$learner_predict_types[[task_type]]
  compatible = keep(names(ordering), function(pt) all(predict_types %in% ordering[[pt]]))
  if (!length(compatible)) {
    stopf("No predict type satisfies %s", str_collapse(predict_types, quote = "'"))
  }
  compatible[which.min(lengths(ordering[compatible]))]
}

generate_initial_design = function(method, search_space, size) {
  internal_tune_ids = search_space$ids(any_tags = "internal_tuning")

  # internal-tuning params are dropped from the design
  design_space = search_space$clone(deep = TRUE)$subset(setdiff(search_space$ids(), internal_tune_ids))

  switch(
    method,
    "sobol" = generate_design_sobol(design_space, size)$data,
    "lhs" = generate_design_lhs(design_space, size)$data,
    "random" = generate_design_random(design_space, size)$data,
    stopf("Unknown initial design method '%s'", method)
  )
}
