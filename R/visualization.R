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
  measure = archive$codomain$data$id[[1]]
  
  .data = NULL
  n_configs = nrow(archive$data) 
  ggplot2::ggplot(data = archive$data, ggplot2::aes(
      x = seq_len(n_configs),
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

  # there should only be one objective, e.g. `classif.ce`
  measure = archive$cols_y

  data = na.omit(archive$data, cols = c(x, y))

  .data = NULL

  # no param provided for y
  if (is.null(y)) {
    g = ggplot2:: ggplot(data = data, ggplot2::aes(
      x = .data[[x]],
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
      x = .data[[x]],
      y = .data[[y]],
      fill = .data[[measure]]
    )) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(title = "Marginal plot") +
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
    if (sd(x, na.rm = TRUE) > 0) {
      (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    } else {
      rep(0, length(x))
    }
  })]
  data_c = data_c[, lapply(.SD, function(x) {
    if (sd(x, na.rm = TRUE) > 0) {
      (x - mean(unique(x), na.rm = TRUE)) / sd(unique(x), na.rm = TRUE)
    } else {
      rep(0, length(x))
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
