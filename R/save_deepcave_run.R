save_deepcave_run = function(learner, path = "logs/mlr3automl") {
  jsonlite::write_json(
    get_configspace(learner),
    paste0(path, "/configspace.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
  
  jsonlite::write_json(
    get_configs(learner),
    paste0(path, "/configs.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )

  jsonlite::write_json(
    get_history(learner),
    paste0(path, "/history.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )

  jsonlite::write_json(
    get_meta(learner),
    paste0(path, "/meta.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
  
  # origins.json
  origins = rep(list(NULL), learner$instance$archive$n_evals)
  names(origins) = seq(learner$instance$archive$n_evals) - 1
  jsonlite::write_json(
    origins,
    paste0(path, "/origins.json"),
    pretty = TRUE, null = "null"
  )
}

get_configspace = function(learner) {
  n_params = nrow(learner$instance$search_space$data)

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
      # TBD: default
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
    # TBD: default
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
    
    # `cond` (below) is a list of `Condition`s. Currently, there are only 'CondEqual' and 'CondAnyOf',
    # which should not be used simultaneously. So this list should always contain only one entry.
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

get_configs = function(learner){
  list(TBD = "TBD")
}

get_history = function(learner){
  list(TBD = "TBD")
}

get_meta = function(learner){
  list(TBD = "TBD")
}
