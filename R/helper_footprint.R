#' @title Encode configs table
#' 
#' @description DeepCAVE relies on [ConfigSpace](`https://github.com/automl/ConfigSpace/tree/main`) to transform configuartions into numerical vectors. This function implements the encoding procedure of Configspace.
#' 
#' @param configs (`[data.table::data.table]`)
#'   Configs to encode, either generated via `sample_random_configs` or `sample_border_configs` or extracted from the tuning archive via `archive$data[, archive$cols_x, with = FALSE]`.
#' @param configspace (`[paradox::ParamSet]`)
#'   Configuration space, e.g. `archive$search_space`.
encode_configs = function(configs, configspace) {
  assert_data_table(configs)
  assert_param_set(configspace)
  
  imap_dtc(configs, function(param_vals, param_id) {
    is_numerical = configspace$class[[param_id]] %in% c("ParamDbl", "ParamInt")
    # numerical values should be normalized
    # What deepcave does:
    #   1. Get param values of the config via `Configuration.get_array`. (https://github.com/automl/DeepCAVE/blob/58d6801508468841eda038803b12fa2bbf7a0cb8/deepcave/runs/__init__.py#L1217)
    #   2. Under the hood, this function returns the `_vector` attribute of the `UniformFloatHyperparameter` (or some other uniform hyperparam). (https://github.com/automl/ConfigSpace/blob/d5b82ae3767f389d340e7a1b2d24a04ea3669f0f/src/ConfigSpace/configuration.py#L166)
    #   3. Each entry of `_vector` is resulted from calling the `to_vector` method of the `UniformFloatHyperparameter`. (https://github.com/automl/ConfigSpace/blob/d5b82ae3767f389d340e7a1b2d24a04ea3669f0f/src/ConfigSpace/configuration.py#L124) This method in turn calls the `to_vector` method of the associated `Transformer`. (https://github.com/automl/ConfigSpace/blob/d5b82ae3767f389d340e7a1b2d24a04ea3669f0f/src/ConfigSpace/hyperparameters/hyperparameter.py#L374)
    #   4. `UniformFloatHyperparameter` has the transformer `UnitScaler` (https://github.com/automl/ConfigSpace/blob/d5b82ae3767f389d340e7a1b2d24a04ea3669f0f/src/ConfigSpace/hyperparameters/uniform_float.py#L98). Its `to_vector` method normalizes the param value to [0,1].
    if (is_numerical) {
      lower = configspace$lower[[param_id]]
      upper = configspace$upper[[param_id]]
      param_vals = (param_vals - lower) / (upper - lower)
    }

    # NaNs should be encoded as -0.2 (-0.5 IS WRONG!!!!)
    param_vals[is.na(param_vals)] = -0.2

    # Categorical values should be between 0..1
    if (!is_numerical) {
      # DeepCAVE factor levels start with 0
      param_vals = as.numeric(factor(param_vals)) - 1
      param_vals = param_vals / (configspace$nlevels[[param_id]] - 1)
    }
    
    return(param_vals)
  })
}


#' @title Implementation of `Footprint._get_depth`
#' 
#' @description See https://github.com/automl/DeepCAVE/blob/58d6801508468841eda038803b12fa2bbf7a0cb8/deepcave/evaluators/footprint.py#L524.
#' 
#' @param configspace (`[paradox::ParamSet]`)
get_depth = function(configspace) {
  depth = table(configspace$deps$id) + 1
  depth = c(branch.selection = 1, depth)
  return(depth)
}


#' @title Implementation of `Footprint._get_distance`
#' 
#' @description See https://github.com/automl/DeepCAVE/blob/58d6801508468841eda038803b12fa2bbf7a0cb8/deepcave/evaluators/footprint.py#L371. All params must be vectors of the same length.
#' 
#' @param config1 (`numeric`)
#' @param config2 (`numeric`)
#' @param is_categorical (`logical`)
#' @param depth (`numeric`)
#'   Obtained by calling `[get_depth]`.
get_distance = function(config1, config2, is_categorical, depth) {
  assert_true(length(config1) == length(config2))
  d = abs(config1 - config2)
  d[is.nan(d)] = 1
  d[is_categorical & (d != 0)] = 1
  sum(d / depth)
}


#' @title  Implementation of the `sample_random_config` function in DeepCAVE
#' 
#' @description See https://github.com/automl/DeepCAVE/blob/58d6801508468841eda038803b12fa2bbf7a0cb8/deepcave/utils/configspace.py#L79.
#' @param configspace (`[paradox::ParamSet]`)
#' @param d (`integer(1)`)\cr
#'   Number of discretized values, same as in `sample_random_config`
#' @param n (`integer(1)`)\cr
#'   Sample size.
sample_random_config = function(configspace, d, n) {
  assert_param_set(configspace)
  assert_count(d, positive = TRUE, null.ok = TRUE)
  assert_count(n, positive = TRUE)

  if (is.null(d)) {
    return(generate_design_random(configspace, n)$data)
  }

  subspaces = configspace$subspaces()
  samples = map_dtc(subspaces, function(subspace) {
    # take the first and only column from the grid design data.table
    # to get a vector to sample from
    possible_values = generate_design_grid(subspace, resolution = d)$data[[1]]
    sample(possible_values, n, replace = TRUE)
  })

  return(samples)
}
