#' @title Cost-Over-Time Plot
#' 
#' @description
#' 
#' @template archive
#' @template theme
#' 
#' @export
cost_over_time = function(archive, theme = ggplot2::theme_minimal()) {
  # there should only be one objective, e.g. `classif.ce`
  measure = archive$cols_y
  
  .data = NULL
  ggplot2::ggplot(data = archive$data, ggplot2::aes(
      x = seq_row(archive$data),
      y = .data[[measure]]
    )) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = "Cost over time",
      x = "configuration ID"
    ) +
    theme
}

#' @title Marginal Plot for Hyperparameters
#' 
#' @description
#' 
#' @template instance
#' @param x (`character(1)`)
#'  Name of the parameter to be mapped to the x-axis.
#' @param y (`character(1)`)
#'  Name of the parameter to be mapped to the y-axis.
#'  If `NULL` (default), the measure (e.g. `classif.ce`) is mapped to the y-axis.
#' @template theme
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
    ggplot2::labs(title = "Marginal plot") +
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
    ggplot2::labs(title = "Marginal plot", x = x) +
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
#' @template archive
#' @param cols_x (`character()`)
#'  Column names of x values.
#'  By default, all untransformed x values from the search space are plotted.
#' @param trafo (`character(1)`)
#'  If `FALSE` (default), the untransformed x values are plotted.
#'  If `TRUE`, the transformed x values are plotted.
#' @template theme
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
#' @param instance ([TuningInstanceSingleCritAsync])
#'  Tuning instance, e.g., stored in the field `$instance` of any `mlr3automl` learner.
#' @param x (`character(1)`)
#'  Name of the parameter to be mapped to the x-axis.
#' @param y (`character(1)`)
#'  Name of the parameter to be mapped to the y-axis.
#'  If `NULL` (default), the measure (e.g. `classif.ce`) is mapped to the y-axis.
#' @param grid_size (`numeric(1)` | `numeric(2)`)
#'  The size of the grid. See `grid.size` of `[iml::FeatureEffect]`.
#' @param center_at (`numeric(1)`)
#'  Value at which the plot was centered. Ignored in the case of two features.
#'  See `center.at` of `[iml::FeatureEffect]`.
#' @param type (`character(1)`)
#'  Type of the two-parameter partial dependence plot. Possible options are listed below.
#'  \itemize{
#'    \item `"contour"`: Create a contour plot using `[ggplot2::geom_contour_filled]`. Only supported if both parameters are numerical.
#'    \item `"heatmap"`: Create a heatmap using `[ggplot2::geom_raster]`. This is the default setting in `iml`
#'  }
#'  Ignored if only one parameter is provided.
#' @template theme
#'
#' @export
partial_dependence_plot = function(
  instance, x, y = NULL, grid_size = 20, center_at = NULL,
  type = "heatmap",
  theme = ggplot2::theme_minimal()
) {
  archive = instance$archive
  param_ids = c(x, y)
  objective = archive$cols_y
  assert_subset(param_ids, archive$cols_x)

  branch = tstrsplit(param_ids, "\\.")[[1]]
  branch = unique(branch)
  if (length(branch) > 1) {
    stop("Parameters from different branches cannot be plotted in the same PDP.")
  }
  
  if (!is.null(y)) {
    assert_choice(type, c("contour", "heatmap"))
  }

  non_numeric = some(param_ids, function(param_id) {
    !is.numeric(archive$data[[param_id]])
  })
  if (non_numeric && type == "contour") {
    stop("Contour plot not supported for non-numeric parameters")
  }

  # iml does not allow logical columns, so encode into factor
  # NOT WORKING
  # walk(param_ids, function(param_id) {
  #   if (is.logical(archive$data[[param_id]])) {
  #     set(archive$data, j = param_id, value = as.factor(archive$data[[param_id]]))
  #   }
  # })

  surrogate = default_surrogate(instance)
  surrogate$archive = archive
  surrogate$update()

  predictor = iml::Predictor$new(
    model = surrogate,
    data = as.data.table(archive)[branch.selection == branch, param_ids, with = FALSE],
    y = as.data.table(archive)[branch.selection = branch, objective, with = FALSE]
  )

  eff = iml::FeatureEffect$new(
    predictor,
    param_ids,
    method = "pdp",
    center.at = center_at,
    grid.size = grid_size
  )

  .data = NULL

  if (is.null(y)) {
    g = eff$plot() +
      ggplot2::scale_fill_viridis_c(direction = -1) +
      ggplot2::labs(fill = measure) +
      theme
    return(g)
  }

  g = switch(type,

    contour = ggplot2::ggplot(eff$results, ggplot2::aes(
        x = .data[[x]], y = .data[[y]], z = .data$.value
      )) +
      ggplot2::geom_contour_filled() +
      ggplot2::scale_fill_viridis_d(direction = -1),

    heatmap = ggplot2::ggplot(eff$results, ggplot2::aes(
        x = .data[[x]], y = .data[[y]],
        fill = .data$.value, color = .data$.value
      )) +
      ggplot2::geom_raster() +
      ggplot2::scale_fill_viridis_c(direction = -1)
  )
  
  g + ggplot2::labs(fill = measure) + theme
}
