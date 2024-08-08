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
  if (!length(instance$result_learner_param_vals)) {
    warning("No run is saved, because no tuning has been completed.")
    return()
  }

  # create a subfolder for saving the current run
  # original Python implementation see `Recorder._set_path()`
  # (https://github.com/automl/DeepCAVE/blob/main/deepcave/runs/recorder.py)
  if (!overwrite) {
    new_idx = 0
    for (fn in list.files(path)) {
      if (!startsWith(fn, "prefix")) next
      idx = last(strsplit(fn, "_")[[1]])
      if (is.numeric(idx)) {
        idx_int = as.integer(idx)
        if (idx_int > new_idx) {
          new_idx = idx_int
        }
      }
    }
    new_idx = new_idx + 1
    run_path = file.path(path, paste0(prefix, "_", new_idx))
  }

  jsonlite::write_json(
    get_configspace(instance),
    paste0(run_path, "/configspace.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
  
  jsonlite::write_json(
    get_configs(instance),
    paste0(run_path, "/configs.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )

  jsonlite::write_json(
    get_history(instance),
    paste0(run_path, "/history.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )

  jsonlite::write_json(
    get_meta(instance),
    paste0(run_path, "/meta.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
  
  # create `origins.json` (a list of `null`s)
  origins = rep(list(NULL), instance$instance$archive$n_evals)
  names(origins) = seq(instance$instance$archive$n_evals) - 1
  jsonlite::write_json(
    origins,
    paste0(run_path, "/origins.json"),
    pretty = TRUE, null = "null"
  )
}


# Prepare the list for converting to `configs.json`
get_configs = function(learner){
  list(TBD = "TBD")
}


# Prepare the list for converting to `configspace.json`
get_configspace = function(instance) {
  n_params = nrow(instance$search_space$data)

  hyperparameters_list = lapply(seq_len(n_params), function(i) {
    row = search_space$data[i, ]
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
        weights = NULL))
    }

    # int / float params
    is_logscale = search_space$is_logscale[[name]]
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
      upper = upper))
  })

  conditions_list = lapply(seq_len(n_params), function(i) {
    row = search_space$deps[i, ]
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


get_history = function(learner){
  list(TBD = "TBD")
}


get_meta = function(learner){
  list(TBD = "TBD")
}
