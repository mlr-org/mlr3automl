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
#' @examples
#' \dontrun{
#' rush_plan(n_workers = 2)
#' task = tsk("penguins")
#' 
#' learner1 = lrn("classif.auto",
#'   learner_ids = c("svm", "ranger"),
#'   small_data_size = 1,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = trm("evals", n_evals = 6)
#' )
#' learner1$train(task)
#' # save to `logs/mlr3automl/run_1`
#' save_deepcave_run(learner1$instance)
#' 
#' # save to `logs/mlr3automl/run`
#' # if this folder already exists, it will be overwritten
#' save_deepcave_run(learner1$instance, overwrite = TRUE)
#' 
#' learner2 = lrn("classif.auto",
#'   learner_ids = c("catboost", "xgboost"),
#'   small_data_size = 1,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = trm("evals", n_evals = 6)
#' )
#' learner2$train(task)
#' # save to `logs/mlr3automl/run_2`
#' save_deepcave_run(learner2$instance)
#' }
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
    for (fn in list.files(path)) {
      if (!startsWith(fn, prefix)) next

      splitted = strsplit(fn, "_")[[1]]
      if (length(splitted) == 1) next # no run index attached

      idx = suppressWarnings(last(splitted))
      if (is.na(idx)) next # idx cannot be coerced to a number

      idx_int = as.integer(idx)
      if (idx_int > new_idx) {
        new_idx = idx_int
      }
    }

    new_idx = new_idx + 1
    run_path = file.path(path, paste0(prefix, "_", new_idx))

    dir.create(run_path, recursive = TRUE)
  } else {
    run_path = file.path(path, prefix)
    if (file.exists(run_path)) {
      lapply(list.files(run_path, full.names = TRUE), file.remove)
    } else{
      dir.create(run_path, recursive = TRUE)
    }
  }


  # `configspace.json`
  jsonlite::write_json(
    get_configspace(instance),
    file.path(run_path, "configspace.json"),
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
    # objectives must be a list, so do not auto unbox if a list has only one entry
    auto_unbox = FALSE,
    pretty = TRUE, null = "list", na = "null",
    dataframe = "values",
    verbose = FALSE
  )
  close(con)
  
  # `origins.json` (a list of `null`s)
  origins = rep(list(NULL), nrow(instance$archive$data))
  names(origins) = seq(nrow(instance$archive$data)) - 1
  jsonlite::write_json(
    origins,
    file.path(run_path, "origins.json"),
    pretty = TRUE, null = "null"
  )
}


# Prepare the list for converting to `configs.json`
get_configs = function(instance){
  archive = instance$archive
  param_ids = archive$cols_x

  # skip branch.selection if there is only one level
  id = NULL # resolve global variable note in R CDM check
  nbranches = length(unique(archive$data$branch.selection))
  if (nbranches == 1) {
    param_ids = setdiff(param_ids, "branch.selection")
  }

  config_table = instance$archive$data[, param_ids, with = FALSE]
  # param values in deepcave are on the original scale, not the log scale
  logscale_params = param_ids[instance$search_space$is_logscale[param_ids]]
  config_table[, (logscale_params) := lapply(.SD, exp), .SDcols = logscale_params]

  configs_list = map(seq_row(config_table), function(i) {
    discard(as.list(config_table[i, ]), is.na)
  })
  names(configs_list) = seq_along(configs_list) - 1

  return(configs_list)
}


# Prepare the list for converting to `configspace.json`
get_configspace = function(instance) {
  param_ids = instance$archive$cols_x

  hyperparameters_list = map(param_ids, function(param_id) {
    id = NULL # resolve global variable note in R CDM check
    row = instance$search_space$data[id == param_id, ]

    # skip branch.selection if there is only one branch
    if (param_id == "branch.selection" && row[["nlevels"]] == 1) return()

    type = switch(row[["class"]],
      ParamFct = "categorical",
      ParamLgl = "categorical",
      ParamDbl = "uniform_float",
      ParamInt = "uniform_int")
    
    # categorical params
    if (type == "categorical") {
      choices = unlist(row[["levels"]])
      return(list(
        name = param_id,
        type = type,
        choices = choices,
        # FIXME: `default` is wrong
        default = choices[[1]],
        probabilities = NULL
      ))
    }

    # int / float params
    is_logscale = instance$search_space$is_logscale[[param_id]]
    lower = row[["lower"]]
    upper = row[["upper"]]
    # FIXME: default is wrong
    default = lower
    if (is_logscale) {
      lower = exp(lower)
      upper = exp(upper)
      default = exp(default)
    }
    return(list(
      name = param_id,
      type = type,
      log = is_logscale,
      lower = lower,
      upper = upper,
      default = default,
      q = NULL
    ))
  })
  # skipping branch.selection results in null entries => discard them
  hyperparameters_list = discard(hyperparameters_list, is.null)


  conditions_list = map(setdiff(param_ids, "branch.selection"), function(param_id) {
    id = NULL # resolve global variable note in R CDM check
    dependency = instance$search_space$deps[id == param_id, ]
    # `svm.degree` and `svm.gamma` depends on `svm.kernel` as well as `branch.selection`.
    # DeepCAVE does not allow one parameter to be conditioned on multiple others.
    # So remove their dependency on `branch.selection`.
    if (nrow(dependency) > 1) {
      on = NULL # resolve global variable note in R CDM check
      dependency = dependency[on != "branch.selection", ]
    }
    child = param_id
    parent = dependency[["on"]]

    # remove dependency on branch.selection if there is only one branch
    nbranches = length(unique(archive$data$branch.selection))
    if (parent == "branch.selection" && nbranches == 1) return()
    
    # `cond` below is a list of `Condition`s.
    # Currently, there are only 'CondEqual' and 'CondAnyOf', which should not be used simultaneously.
    # So this list should always contain only one entry.
    cond = dependency[["cond"]][[1]]
    if (class(cond)[[1]] == "CondEqual") {
        return(list(child = child, parent = parent, type = "EQ", value = cond$rhs))
    }
    return(list(child = child, parent = parent, type = "IN", values = cond$rhs))
  })
  # skipping branch.selection results in null entries => discard them
  conditions_list = discard(conditions_list, is.null)

  return(list(
    hyperparameters = hyperparameters_list,
    conditions = conditions_list,
    forbiddens = list()
  ))
}

# Prepare the data.table for converting to `history.jsonl`
get_history = function(instance) {
  costs = instance$archive$cols_y

  selected_cols = c(costs, "timestamp_xs", "timestamp_ys", "state")
  timestamp_xs = timestamp_ys = state = NULL # resolve global variable note in R CDM check
  history_table = instance$archive$data[, selected_cols, with = FALSE][, list(
    config_id = seq_row(instance$archive$data) - 1,
    budget = 0,
    seed = -1,
    # combine costs into a list column
    costs = lapply(transpose(.SD), c),
    # handle start and end time (time elapsed since first timestamp)
    # see https://github.com/automl/DeepCAVE/blob/main/deepcave/runs/recorder.py
    # start and end time here might having different meanings than the original implementation
    start_time = as.numeric(timestamp_xs - timestamp_xs[1]),
    end_time = as.numeric(timestamp_ys - timestamp_ys[1]),
    # state is either "finished" <=> SUCESS = 1 or ABORTED = 5
    # see https://github.com/automl/DeepCAVE/blob/main/deepcave/runs/status.py
    state = ifelse(state == "finished", 1, 5),
    additionals = list()
  ), .SDcols = costs]

  return(history_table)
}


# Prepare the list for converting to 'meta.json'
get_meta = function(instance){
  costs = instance$archive$cols_y

  objectives_list = map(costs, function(cost) {
    measure = msr(cost)

    lower = measure$range[[1]]
    if (is.finite(lower)) {
      lock_lower = TRUE
    } else {
      lower = min(instance$archive$data[, cost, with = FALSE])
      lock_lower = FALSE
    }

    upper = measure$range[[2]]
    if (is.finite(upper)) {
      lock_upper = TRUE
    } else {
      upper = max(instance$archive$data[, cost, with = FALSE])
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
    budgets = list(0),
    seeds = list(-1)
  ))
}
