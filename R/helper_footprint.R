#' @title Encode configs table
#' 
#' @description DeepCAVE relies on [ConfigSpace](`https://github.com/automl/ConfigSpace/tree/main`) to transform configuartions into numerical vectors. This function implements the encoding procedure of Configspace.
#' 
#' @param configs (`[data.table::data.table]`)
#'   Configs to encode, either generated via `sample_random_configs` or `sample_border_configs` or extracted from the tuning archive via `archive$data[, archive$cols_x, with = FALSE]`.
#' @param configspace (`[paradox::ParamSet]`)
#'   Configuration space, e.g. `archive$search_space`.
#' 
#' @return `[data.table::data.table]` of the same size as `configs`.
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
#' @return `numeric()` of the same length as the number of params in `configspace`.
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
#'   Obtained by calling `[get_depth()]`.
#' 
#' @return `numeric(1)`
get_distance = function(config1, config2, is_categorical, depth) {
  assert_true(length(config1) == length(config2))
  d = abs(config1 - config2)
  d[is.nan(d)] = 1
  d[is_categorical & (d != 0)] = 1
  sum(d / depth)
}

#' @title Implementation of `Footprint._get_max_distance`
#' 
#' @description See https://github.com/automl/DeepCAVE/blob/58d6801508468841eda038803b12fa2bbf7a0cb8/deepcave/evaluators/footprint.py#L349. Just counts the number of numerical params.
#' 
#' @param configspace (`[paradox::ParamSet]`)
#' 
#' @return `numeric(1)`
get_max_distance = function(configspace) {
  sum(configspace$is_number)
}


#' @title Discretized 1D Uniform Sampler
#' 
#' @description
#' Implements the DeepCAVE samplers `[sample_random_config](https://github.com/automl/DeepCAVE/blob/58d6801508468841eda038803b12fa2bbf7a0cb8/deepcave/utils/configspace.py#L79)` and `[sample_border_config](https://github.com/automl/DeepCAVE/blob/58d6801508468841eda038803b12fa2bbf7a0cb8/deepcave/utils/configspace.py#L36)`.
#' 
#' @param param ([`ParamSet`])\cr
#'   Domain / support of the distribution we want to sample from.
#'   Must be one-dimensional.
#' 
Sampler1DUnifDisc = R6Class("Sampler1DUnifDisc", inherit = paradox::Sampler1D,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param d (`numeric(1)` | NULL)\cr
    #'   Number (>= 2) of discrete values to sample from. If `d = 2`, one gets the border sampler.
    #'   Ignored for categorical params.
    initialize = function(param, d = NULL) {
      super$initialize(param)
      assert_param_set(self$param, no_untyped = TRUE, must_bounded = TRUE)
      private$.d = d

      if (self$param$is_categ) {
        private$.choices = self$param$levels[[1]]
      } else {
        private$.choices = generate_design_grid(param, resolution = d)$data[[1]]
      }
    }
  ),

  active = list(
    #' @field d (`numeric(1)` | NULL)\cr
    #'   Number of discrete values to sample from.
    d = function(v) if (missing(v)) private$.d else private$.d = assert_int(d, lower = 2),

    #' @field choices (`numeric()` | `integer()` | `factor()` | `logical()`)\cr
    #'   Possible values to sample from.
    choices = function(v) if (missing(v)) private$.choices else stop("Cannot overwrite choices. Change d instead.")
  ),

  private = list(
    .d = NULL,
    .choices = NULL,
    .sample = function(n) {
      s = sample(private$.choices, n, replace = TRUE)
      super$as_dt_col(s)
    }
  )
)

#' @title Get Border/Random Generator
#' 
#' @description Creates a `SamplerHierarchical` for generating border/random configs.
#'
#' @param d (`numeric(1)` | NULL)\cr
#'   Number of discrete values to sample from. If `NULL`, all values are used. If `d = 2`, only borders are used.
#' 
#' @return `SamplerHierarchical`.
get_generator = function(configspace, d = NULL) {
  assert_int(d, lower = 2, null.ok = TRUE)

  if (is.null(d)) {
    return(SamplerUniform$new(configspace))
  }

  samplers = map(configspace$subspaces(), function(subspace) {
    Sampler1DUnifDisc$new(subspace, d)
  })
  return(SamplerHierarchical$new(configspace, samplers))
}

#' @title Get new distances
#' 
#' @description If new_config is not rejected, return a vector of distances between new_config and old configs. If new_config is rejected, return NULL.
get_new_distances = function(configs, new_config, metric, rejection_threshold) {
  new_distances = numeric(nrow(configs))
  for (i in seq_row(configs)) {
    old_config = unlist(configs[i, ])
    d = metric(old_config, new_config)
    if (d < rejection_threshold) {
      return(NULL)
    }
    new_distances[[i]] = d
  }

  return(new_distances)
}
