generate_default_design = function(task_type, learner_ids, task, tuning_space, branch = TRUE) {
  map_dtr(learner_ids, function(learner_id) {
    if (paste0(task_type, ".", learner_id) %nin% mlr_learners$keys()) {
      return(data.table(branch.selection = learner_id))
    }

    learner = lrn(sprintf("%s.%s", task_type, learner_id))
    token = tuning_space[[learner_id]]

    # learner without tuning space
    if (!length(token)) {
      return(data.table(branch.selection = learner_id))
    }

    names(token) = gsub(paste0("^", learner_id, "."), "", names(token))
    learner$param_set$set_values(.values = token)
    search_space = learner$param_set$search_space()
    internal_tune_ids = search_space$ids(any_tags = "internal_tuning")
    if (length(internal_tune_ids)) {
      search_space = search_space$subset(setdiff(search_space$ids(), internal_tune_ids))
    }

    xss = default_values(learner, search_space = search_space, task = task)
    has_logscale = map_lgl(search_space$params$.trafo, function(x) identical(x, exp))
    xdt = as.data.table(map_if(xss, has_logscale, function(value) if (value > 0) log(value) else value))

    setnames(xdt, sprintf("%s.%s", learner_id, names(xdt)))

    if (learner_id == "fastai") {
      xdt = cbind(xdt, data.table(fastai.n_layers = "2", fastai.layer_size_1 = 200, fastai.layer_size_2 = 200))
    }
    if (branch) {
      set(xdt, j = "branch.selection", value = learner_id)
    }
    xdt
  }, .fill = TRUE)
}

generate_lhs_design = function(size, task_type, learner_ids, tuning_space, branch = TRUE) {
  if (!size) return(data.table())
  learner_ids = learner_ids[learner_ids %in% names(tuning_space)]

  map_dtr(learner_ids, function(learner_id) {
    learner = lrn(sprintf("%s.%s", task_type, learner_id))

    token = tuning_space[[learner_id]]
    # learner without tuning space
    if (!length(token)) {
      return(data.table(branch.selection = learner_id))
    }

    names(token) = gsub(paste0("^", learner_id, "."), "", names(token))
    learner$param_set$set_values(.values = token)
    search_space = learner$param_set$search_space()
    internal_tune_ids = search_space$ids(any_tags = "internal_tuning")
    if (length(internal_tune_ids)) {
      search_space = search_space$subset(setdiff(search_space$ids(), internal_tune_ids))
    }

    if (learner_id == "fastai") {
      search_space = c(search_space, fastai_search_space)
    }

    # if there are categorical parameters, we need to sample at least the maximum number of levels
    n_levels = search_space$nlevels[search_space$ids(class = c("ParamFct", "ParamLgl"))]
    xdt = generate_design_lhs(search_space, max(n_levels, size))$data
    setnames(xdt, sprintf("%s.%s", learner_id, names(xdt)))
    if (branch) {
      set(xdt, j = "branch.selection", value = learner_id)
    }
    xdt
  }, .fill = TRUE)
}

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

  callback = function(env) {
    if (!exists("start_time")) start_time <<- Sys.time()

    if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
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

