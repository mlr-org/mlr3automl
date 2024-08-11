#' @title Save Tuning History as a DeepCAVE Run
#' 
#' @description
#' Exports information stored in a `TuningInstance` in a format recognized by [DeepCAVE](https://automl.github.io/DeepCAVE/main/index.html) as a run. Each run is stored as a folder containing five files `configs.json`, `configspace.json`, `history.jsonl`, `meta.json`, and `origins.json`.
#' 
#' @param instance ([TuningInstanceAsyncSingleCrit])
#' Tuning instance to save.
#' 
#' @param path (`character(1)`)
#' Path to save the run. Defaults to `"logs/mlr3automl`.
#' 
#' @param prefix (`character(1)`)
#' Prefix for the name of a new subfolder under `path` for storing the current run.
#' 
#' @param overwrite (`character(1)`)
#' If `FALSE` (default), creates a new subfolder to save the current run. If `TRUE`, all existing runs will be deleted.
#' 
#' @export
save_deepcave_run = function(instance, path = "logs/mlr3automl", prefix = "run", overwrite = FALSE) {
  # don't save untuned instance
  if (is.null(instance$result_learner_param_vals)) {
    warning("No run is saved, because no tuning has been completed.")
    return()
  }

  # create a subfolder for saving the current run
  # original Python implementation see `Recorder._set_path()`
  # (https://github.com/automl/DeepCAVE/blob/main/deepcave/runs/recorder.py)
  if (!overwrite) {
    new_idx = 0
    walk(list.files(path), function(fn) {
      if (!startsWith(fn, "prefix")) return()
      idx = last(strsplit(fn, "_")[[1]])
      if (is.numeric(idx)) {
        idx_int = as.integer(idx)
        if (idx_int > new_idx) {
          new_idx = idx_int
        }
      }
    })
    new_idx = new_idx + 1
    run_path = file.path(path, paste0(prefix, "_", new_idx))
    dir.create(run_path)
  } else {
    run_path = file.path(path, prefix)
    if (file.exists(run_path)) {
      lapply(list.files(run_path, full.names = TRUE), file.remove)
    } else{
      dir.create(run_path)
    }
  }


  # `configspace.json`
  jsonlite::write_json(
    get_configspace(instance),
    paste0(run_path, "/configspace.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )

  # `configs.json` 
  jsonlite::write_json(
    get_configs(instance),
    file.path(run_path, "configs.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )

  # `meta.json`
  jsonlite::write_json(
    get_meta(instance),
    file.path(run_path, "meta.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )

  # `history.jsonl` 
  con = file(file.path(run_path, "history.jsonl"), open = "w")
  jsonlite::stream_out(
    get_history(instance),
    con,
    auto_unbox = TRUE, pretty = TRUE, null = "list", na = "null",
    dataframe = "values"
  )
  close(con)
  
  # `origins.json` (a list of `null`s)
  origins = rep(list(NULL), instance$archive$n_evals)
  names(origins) = seq(instance$archive$n_evals) - 1
  jsonlite::write_json(
    origins,
    paste0(run_path, "/origins.json"),
    pretty = TRUE, null = "null"
  )
}


# Prepare the lists for converting to `configs.json`
get_configs = function(instance){
  param_ids = instance$search_space$data[, id]

  configs_list = map(seq_len(instance$archive$n_evals), function(i) {
    row = as.list(instance$archive$data[i, ])
    tuned_params = grep(paste0("^", row[["branch.selection"]]), param_ids, value = TRUE)
    walk(tuned_params, function(param) {
      if (instance$search_space$is_logscale[[param]]) {
        row[[param]] = exp(row[[param]])
      }
    })
    return(row[c("branch.selection", tuned_params)])
  })
  
  names(configs_list) = seq_along(configs_list) - 1

  return(configs_list)
}


# Prepare the list for converting to `configspace.json`
get_configspace = function(instance) {
  n_params = nrow(instance$search_space$data)

  hyperparameters_list = map(seq_len(n_params), function(i) {
    row = instance$search_space$data[i, ]
    name = row[["id"]]
    type = switch(row[["class"]],
      ParamFct = "categorical",
      ParamLgl = "categorical",
      ParamDbl = "uniform_float",
      ParamInt = "uniform_int")
    
    # categorical params
    if (type == "categorical") {
      choices = unlist(row[["levels"]])
      # FIXME: the entry `default` is missing
      return(list(
        name = name,
        type = type,
        choices = choices,
        weights = NULL
      ))
    }

    # int / float params
    is_logscale = instance$search_space$is_logscale[[name]]
    lower = row[["lower"]]
    upper = row[["upper"]]
    if (is_logscale) {
      lower = exp(lower)
      upper = exp(upper)
    }
    # FIXME: the entry `default` entry is missing
    return(list(
      name = name,
      type = type,
      log = is_logscale,
      lower = lower,
      upper = upper
    ))
  })

  conditions_list = map(seq_len(n_params), function(i) {
    row = instance$search_space$deps[i, ]
    child = row[["id"]]
    parent = row[["on"]]
    
    # `cond` below is a list of `Condition`s.
    # Currently, there are only 'CondEqual' and 'CondAnyOf', which should not be used simultaneously.
    # So this list should always contain only one entry.
    cond = row[["cond"]][[1]]
    if (is(cond, "CondEqual")) {
        return(list(child = child, parent = parent, type = "EQ", value = cond$rhs))
      }
      return(list(child = child, parent = parent, type = "IN", values = cond$rhs))
  })

  return(list(
    hyperparameters = hyperparameters_list,
    conditions = conditions_list,
    forbiddens = list()
  ))
}

get_history = function(instance) {
  selected_cols = c(costs, "timestamp_xs", "timestamp_ys", "state")
  history_table = instance$archive$data[, ..selected_cols][, .(
    config_id = seq_len(n_evals) - 1,
    budget = 0,
    seed = -1,
    costs = lapply(transpose(.SD), c),
    # handle start and end time (time elapsed since first timestamp)
    # see https://github.com/automl/DeepCAVE/blob/main/deepcave/runs/recorder.py
    start_time = as.numeric(timestamp_xs - timestamp_xs[1]),
    end_time = as.numeric(timestamp_ys - timestamp_ys[1]),
    # state is either "finished" (SUCESS = 1) or "queued" (NOT_EVALUATED = 0)
    # see https://github.com/automl/DeepCAVE/blob/main/deepcave/runs/status.py
    state = ifelse(state == "finished", 1, 6),
    additionals = list()
  ), .SDcols = costs]

  return(history_table)
}

get_meta = function(instance){
  costs = instance$objective$codomain$data[, id]

  objectives_list = map(costs, function(cost) {
    measure = msr(cost)

    lower = measure$range[[1]]
    if (is.finite(lower)) {
      lock_lower = TRUE
    } else {
      lower = min(instance$archive$data[, ..cost])
      lock_lower = FALSE
    }

    upper = measure$range[[2]]
    if (is.finite(upper)) {
      lock_upper = TRUE
    } else {
      upper = max(instance$archive$data[, ..cost])
      lock_upper = FALSE
    }

    optimize = if (measure$minimize) {
      "lower"
    } else {
      "upper"
    }

    return(list(name = cost, lower = lower, upper = upper,
      lock_lower = lock_lower, lock_upper = lock_upper, optimize = optimize))

  })  
  
  return(list(
    objectives = objectives_list,
    budgets = rep(list(0), instance$archive$n_evals),
    seeds = list(-1)
  ))
}
