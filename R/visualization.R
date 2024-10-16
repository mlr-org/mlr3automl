#' @title Cost-Over-Time Plot
#' 
#' @description
#' 
#' @template param_instance
#' @param time (`character(1)`)\cr
#'   Column in the archive to be interpreted as the time variable, e.g. "timestamp_xs", "timestamp_ys".
#'   If `NULL` (default), the configuration ID will be used.
#' @template param_theme
#' 
#' @export
cost_over_time = function(archive, time = NULL, theme = ggplot2::theme_minimal()) {
  # there should only be one objective, e.g. `classif.ce`
  objective = archive$cols_y

  .data = NULL
  if (is.null(time)) {
    x = seq_row(archive$data)
    g = ggplot2::ggplot(data = as.data.table(archive), ggplot2::aes(
      x = x,
      y = .data[[objective]]
    )) +
    ggplot2::labs(x = "configuration ID")
  } else {
    assert_choice(time, names(as.data.table(archive)))
    g = ggplot2::ggplot(data = as.data.table(archive), ggplot2::aes(
      x = .data[[time]],
      y = .data[[objective]]
    ))
  }
  
  
  g + ggplot2::geom_point() +
    ggplot2::geom_line() +
    theme
}

#' @title Marginal Plot for Hyperparameters
#' 
#' @description
#' 
#' @template param_instance
#' @param x (`character(1)`)
#'  Name of the parameter to be mapped to the x-axis.
#' @param y (`character(1)`)
#'  Name of the parameter to be mapped to the y-axis.
#'  If `NULL` (default), the measure (e.g. `classif.ce`) is mapped to the y-axis.
#' @template param_theme
#' 
#' @export
marginal_plot = function(archive, x, y = NULL, theme = ggplot2::theme_minimal()) {
  param_ids = archive$cols_x
  assert_choice(x, param_ids)
  assert_choice(y, param_ids, null.ok = TRUE)

  # use transformed values if trafo is set
  x_trafo = paste0("x_domain_", x)

  # there should only be one objective, e.g. `classif.ce`
  measure = archive$cols_y

  data = na.omit(as.data.table(archive), cols = c(x_trafo, y))

  .data = NULL

  # no param provided for y
  if (is.null(y)) {
    g = ggplot2:: ggplot(data = data, ggplot2::aes(
      x = .data[[x_trafo]],
      y = .data[[measure]]
    )) +
    ggplot2::geom_point(alpha = 0.6) +
    theme

    if (archive$search_space$is_logscale[[x]]) {
      g = g + ggplot2::scale_x_log10()
    }

    return(g)
  }

  # param provided for y
  g = ggplot2::ggplot(data = data, ggplot2::aes(
      x = .data[[x_trafo]],
      y = .data[[y]],
      col = .data[[measure]]
    )) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::scale_color_viridis_c() +
    ggplot2::labs(x = x) +
    theme

  if (archive$search_space$is_logscale[[x]]) {
    g = g + ggplot2::scale_x_log10()
  }
  if (archive$search_space$is_logscale[[y]]) {
    g = g + ggplot2::scale_y_log10()
  }

  return(g)
}


#' @title Parallel Coordinates Plot
#' 
#' @description Adapted from [mlr3viz::autoplot()] with `type == "parallel"`. Since the hyperparameters of each individual learner are conditioned on `branch.selection`, missing values are expected in the archive data. When standardizing the hyperparameter values (referred to as "x values" in the following to be consistent with `mlr3viz` documentation), `na.omit == TRUE` is used to compute `mean()` and `sd()`.
#' 
#' @template param_instance
#' @param cols_x (`character()`)
#'  Column names of x values.
#'  By default, all untransformed x values from the search space are plotted.
#' @param trafo (`character(1)`)
#'  If `FALSE` (default), the untransformed x values are plotted.
#'  If `TRUE`, the transformed x values are plotted.
#' @template param_theme
#'
#' @export
parallel_coordinates = function(archive, cols_x = NULL, trafo = FALSE, theme = ggplot2::theme_minimal()) {
  assert_subset(cols_x, c(archive$cols_x, paste0("x_domain_", archive$cols_x)))
  assert_flag(trafo)

  if (is.null(cols_x)) {
    cols_x = archive$cols_x
  }
  if (trafo) {
    cols_x = paste0("x_domain_", cols_x)
  }
  cols_y = archive$cols_y

  data = as.data.table(archive)
  data = data[, c(cols_x, cols_y), with = FALSE]
  x_axis = data.table(x = seq(names(data)), variable = names(data))

  # split data
  data_l = data[, .SD, .SDcols = which(sapply(data, function(x) is.character(x) || is.logical(x)))]
  data_n = data[, .SD, .SDcols = which(sapply(data, is.numeric))]
  data_y = data[, cols_y, with = FALSE]

  # factor columns to numeric
  data_c = data_l[, lapply(.SD, function(x) as.numeric(as.factor(x)))]

  # rescale
  data_n = data_n[, lapply(.SD, function(x) {
    if (sd(x, na.rm = TRUE) %in% c(0, NA)) {
      rep(0, length(x))
    } else {
      (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    }
  })]
  data_c = data_c[, lapply(.SD, function(x) {
    if (sd(x, na.rm = TRUE) %in% c(0, NA)) {
      rep(0, length(x))
    } else {
      (x - mean(unique(x), na.rm = TRUE)) / sd(unique(x), na.rm = TRUE)
    }
  })]

  # to long format
  set(data_n, j = "id", value = seq_row(data_n))
  set(data_y, j = "id", value = seq_row(data_y))
  data_n = melt(data_n, measure.var = setdiff(names(data_n), "id"))

  if (nrow(data_c)) {
    # Skip if no factor column is present
    set(data_c, j = "id", value = seq_row(data_c))
    data_c = melt(data_c, measure.var = setdiff(names(data_c), "id"))
    data_l = data_l[, lapply(.SD, as.character)] # Logical to character
    data_l = melt(data_l, measure.var = names(data_l), value.name = "label")[, "label"]
    set(data_c, j = "label", value = data_l)
  }

  # merge
  data = rbindlist(list(data_c, data_n), fill = TRUE)
  data = merge(data, x_axis, by = "variable")
  data = merge(data, data_y, by = "id")
  setorderv(data, "x")

  ggplot2::ggplot(data,
    mapping = ggplot2::aes(
      x = .data[["x"]],
      y = .data[["value"]])) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        group = .data$id,
        color = .data[[cols_y]]),
      linewidth = 1) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = x)) +
    {
      if (nrow(data_c)) ggplot2::geom_label(
        mapping = ggplot2::aes(label = .data$label),
        data = data[!is.na(data$label), ])
    } +
    ggplot2::scale_x_continuous(breaks = x_axis$x, labels = x_axis$variable) +
    ggplot2::scale_color_viridis_c() +
    ggplot2::guides(color = ggplot2::guide_colorbar(barwidth = 0.5, barheight = 10)) +
    theme +
    ggplot2::theme(axis.title.x = ggplot2::element_blank())
}


#' @title Partial Dependence Plot
#' 
#' @description Creates a partial dependenc plot (PDP) via the `[iml]` package.
#' 
#' @template param_instance
#' @param x (`character(1)`)
#'  Name of the parameter to be mapped to the x-axis.
#' @param y (`character(1)`)
#'  Name of the parameter to be mapped to the y-axis.
#' @param type (`character(1)`)
#'  Type of the two-parameter partial dependence plot. Possible options are listed below.
#'  \itemize{
#'    \item `"default"`: Use the default setting in `iml`.
#'    \item `"contour"`: Create a contour plot using `[ggplot2::geom_contour_filled]`. Only supported if both parameters are numerical.
#'  }
#'  Ignored if only one parameter is provided.
#' @template param_theme
#' @param ... (anything)
#'   Arguments passed to `[iml::FeatureEffect]`.
#'
#' @export
partial_dependence_plot = function(
  instance, x, y, type = "default",
  theme = ggplot2::theme_minimal()
) {
  archive = instance$archive
  assert_choice(x, archive$cols_x)
  assert_choice(y, archive$cols_x, null.ok = TRUE)

  branch = tstrsplit(c(x, y), "\\.")[[1]]
  branch = unique(branch)
  if (length(branch) > 1) {
    stop("Parameters from different branches cannot be plotted in the same PDP.")
  }
  
  if (!is.null(y)) {
    assert_choice(type, c("contour", "default"))
  }

  non_numeric = some(c(x, y), function(param_id) {
    !is.numeric(archive$data[[param_id]])
  })
  if (non_numeric && type == "contour") {
    stop("Contour plot not supported for non-numeric parameters")
  }

  # prepare data for surrogate model
  archive_data = as.data.table(archive)[, c(archive$cols_x, archive$cols_y), with = FALSE]
  archive_data = archive_data[!is.na(archive_data[[archive$cols_y]]), ]
  archive_data[, archive$cols_x := lapply(.SD, function(col) {
    # iml does not accept lgcl features
    if (is.logical(col)) return(factor(col, levels = c(FALSE, TRUE)))
    # also convert integer to double to avoid imputeoor error
    if (is.integer(col)) return(as.numeric(col))
    return(col)
  }), .SDcols = archive$cols_x]
  task = as_task_regr(archive_data, target = archive$cols_y)

  # train surrogate model
  surrogate = po("imputeoor",
    multiplier = 3,
    affect_columns = selector_type(c("numeric", "character", "factor", "ordered"))
  ) %>>% default_rf()
  surrogate = GraphLearner$new(surrogate)
  surrogate$train(task)

  # # store the data.table format for later use in predict.function
  # prototype = archive_data[0, archive$cols_x, with = FALSE]

  # new data to compute PDP
  pdp_data = generate_design_random(archive$search_space, n = 1e3)$data
  # same type conversion as above
  pdp_data[, archive$cols_x := lapply(.SD, function(col) {
    if (is.logical(col)) return(factor(col, levels = c(FALSE, TRUE)))
    if (is.integer(col)) return(as.numeric(col))
    return(col)
  }), .SDcols = archive$cols_x]
  pdp_data_types = pdp_data[, lapply(.SD, storage.mode)]

  predictor = iml::Predictor$new(
    model = surrogate,
    data = pdp_data[, archive$cols_x, with = FALSE],
    predict.function = function(model, newdata) {
      model$predict_newdata(newdata)$response
    }
  )

  eff = iml::FeatureEffect$new(
    predictor,
    c(x, y),
    method = "pdp"
  )

  .data = NULL

  g = switch(type,

    contour = ggplot2::ggplot(eff$results, ggplot2::aes(
        x = .data[[x]], y = .data[[y]], z = .data$.value
      )) +
      ggplot2::geom_contour_filled() +
      ggplot2::scale_fill_viridis_d(direction = -1),

    # FIXME: rug = TRUE causes error when, e.g., x = "svm.cost", y = "svm.degree"
    # related to the problem that degree is missing for some instances?
    default = eff$plot(rug = FALSE)
  )

  # TBD: remove existing scales, use viridis instead
  g + ggplot2::scale_fill_viridis_c(name = archive$cols_y, direction = -1) + theme
}


#' @title Pareto Front
#' 
#' @description Plots the Pareto front with x-axis representing the tuning objective (e.g. `"classif.ce`) and y-axis representing time (the `runtime_learners` column in the archive).
#' 
#' @template param_instance
#' @template param_theme
#' 
#' @export
pareto_front = function(instance, theme = ggplot2::theme_minimal()) {
  # adopted from `Archive$best()` for multi-crit
  archive = instance$archive
  tab = archive$finished_data
  ymat = t(as.matrix(tab[, c(archive$cols_y, "runtime_learners"), with = FALSE]))
  ymat = archive$codomain$maximization_to_minimization * ymat
  best = tab[!bbotk::is_dominated(ymat)]
  
  .data = NULL
  ggplot2::ggplot() +
    ggplot2::geom_point(data = archive$data,
      ggplot2::aes(x = .data[[archive$cols_y]], y = .data$runtime_learners),
      alpha = 0.2
    ) +
    ggplot2::geom_step(data = best,
      ggplot2::aes(x = .data[[archive$cols_y]], y = .data$runtime_learners)
    ) +
    theme
}


#' @title Configuration Footprint
#' 
#' @description Implements the configuration footprint plot in [DeepCAVE](https://automl.github.io/DeepCAVE/main/plugins/configuration_footprint.html).
#' 
#' @template param_instance
#' @template param_theme
#' 
#' @export
config_footprint = function(instance, theme = ggplot2::theme_minimal()) {
  requireNamespace("smacof", quietly = TRUE)
  set.seed(0)

  archive = instance$archive

  # generate configs
  n_random = nrow(archive$data) * 10
  discretization = 10
  
  evaluated = as.data.table(archive)[, archive$cols_x, with = FALSE]
  set(evaluated, j = "type", value = "evaluated")
  incumbent_key = archive$best()$keys
  set(evaluated, i = which(archive$data$keys == incumbent_key), j = "type", value = "incumbent")

  border = generate_design_grid(archive$search_space, resolution = 2)$data
  set(border, j = "type", value = "border")

  random = sample_random_config(archive$search_space, d = discretization, n = n_random)
  set(random, j = "type", value = "random")
  
  all_configs = rbind(evaluated, border, random)
  all_configs = unique(all_configs, by = archive$cols_x)

  config_type = all_configs$type
  all_configs = all_configs[, -"type", with = FALSE]

  # encode config
  # https://github.com/automl/DeepCAVE/blob/58d6801508468841eda038803b12fa2bbf7a0cb8/deepcave/runs/__init__.py#L1189
  all_configs[, archive$cols_x := lapply(.SD, function(col) {
    # non-numeric to numeric
    if (!is.numeric(col)) {
      col = as.numeric(factor(col))
      # categorical values should be between 0 and 1
      col = col / length(unique(col))
    }
    # NAs should be encoded as -0.5
    col[is.na(col)] = - 0.5
    return(col)
  }), .SDcols = archive$cols_x]
  
  
  # calculate distances

  is_categorical = !archive$data[, lapply(.SD, is.numeric), .SDcols = archive$cols_x]
  is_categorical = unlist(is_categorical)

  # get depth (number of parents + 1) for each param
  # https://github.com/automl/DeepCAVE/blob/58d6801508468841eda038803b12fa2bbf7a0cb8/deepcave/evaluators/footprint.py#L524
  # Theoretically, if a param has two parents and each parent depends on branch.selection,
  # then the param has depth 2. But we don't have such cases (yet).
  # if a param appears twice in the deps table, it has two parents, otherwise one
  # depth := num of parents + 1
  depth = table(archive$search_space$deps$id) + 1
  depth = c(branch.selection = 1, depth)

  # Implementation of the `Footprint._get_distance` function in DeepCAVE
  # https://github.com/automl/DeepCAVE/blob/58d6801508468841eda038803b12fa2bbf7a0cb8/deepcave/evaluators/footprint.py#L371
  # config1, config2: numeric vectors of the same length
  get_distance = function(config1, config2) {
    assert_true(length(config1) == length(config2))
    d = abs(config1 - config2)
    d[is.nan(d)] = 1
    d[is_categorical & (d != 0)] = 1
    sum(d / depth)
  }

  # get pairwise distances
  # rejection and retrying are not yet implemented
  n_configs = nrow(all_configs)
  distances = matrix(NA, nrow = n_configs, ncol = n_configs)
  for (i in seq_len(n_configs - 1)) {
    config1 = unlist(all_configs[i, ])
    for (j in seq(i + 1, n_configs)) {
      config2 = unlist(all_configs[j, ])
      d = get_distance(config1, config2)
      distances[i, j] = d
      distances[j, i] = d
    }
  }

  # MDS
  diss_mat = as.dist(distances)
  footprint = as.data.frame(smacof::mds(diss_mat)$conf)
  setDT(footprint)

  # train on objective
  evaluated = config_type %in% c("evaluated", "incumbent")
  training_data = cbind(
    footprint[evaluated, ],
    archive$data[, archive$cols_y, with = FALSE]
  )
  task = as_task_regr(
    training_data,
    target = archive$cols_y
  )
  surrogate = default_rf()
  surrogate$train(task)

  # train on area is not yet supported

  # predict on grid
  grid_size = 80
  x_min = min(footprint$D1)
  x_max = max(footprint$D1)
  y_min = min(footprint$D2)
  y_max = max(footprint$D2)
  xs = seq(x_min, x_max, length.out = grid_size)
  ys = seq(y_min, y_max, length.out = grid_size)
  grid_points = expand.grid(xs, ys)
  names(grid_points) = c("D1", "D2")
  pred = surrogate$predict_newdata(grid_points)$response
  grid_points[[archive$cols_y]] = pred

  # plot
  set(footprint, j = "config_type", value = config_type)
  set(footprint, j = "is_incumbent", value = config_type == "incumbent")
  .data = NULL
  
  ggplot2::ggplot() +
    ggplot2::geom_raster(
        data = grid_points,
        ggplot2::aes(x = .data$D1, y = .data$D2, fill = .data[[archive$cols_y]])
    ) +
    ggplot2::geom_point(
      data = footprint,
      ggplot2::aes(
        x = .data$D1, y = .data$D2,
        col = .data$config_type,
        shape = .data$is_incumbent
      )
    ) +
    ggplot2::scale_fill_gradient(
      low = "#56B1F7",
      high = "#132B43"
    ) +
    ggplot2::scale_color_manual(
      breaks = c("border", "random", "evaluated", "incumbent"),
      values = c("green", "purple", "orange", "red")
    ) +
    ggplot2::scale_shape_manual(
      breaks = c(FALSE, TRUE),
      values = c(4, 17)
    ) +
    ggplot2::labs(col = "type") +
    ggplot2::guides(shape = "none") +
    theme +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}

# Implementation of the `sample_random_config` function in DeepCAVE
# https://github.com/automl/DeepCAVE/blob/58d6801508468841eda038803b12fa2bbf7a0cb8/deepcave/utils/configspace.py#L79
# d: same as `sample_random_config`
# n: sample size
sample_random_config = function(param_set, d, n) {
  assert_param_set(param_set)
  assert_count(d, positive = TRUE, null.ok = TRUE)
  assert_count(n, positive = TRUE)

  if (is.null(d)) {
    return(generate_design_random(param_set, n)$data)
  }

  subspaces = param_set$subspaces()
  samples = map_dtc(subspaces, function(subspace) {
    # take the first and only column from the grid design data.table
    # to get a vector to sample from
    possible_values = generate_design_grid(subspace, resolution = d)$data[[1]]
    sample(possible_values, n, replace = TRUE)
  })

  return(samples)
}
