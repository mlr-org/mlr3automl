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
  cost = archive$codomain$data$id[[1]]
  
  .data = NULL
  n_configs = nrow(archive$data) 
  ggplot2::ggplot(data = archive$data, ggplot2::aes(
      x = seq_len(n_configs),
      y = .data[[cost]]
    )) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = "Cost over time",
      x = "configuration ID"
    ) +
    theme
}


#' @title 2D Marginal Plot for Hyperparameters
#' 
#' @description
#' 
#' @template instance
#' @param x (`character(1)`)
#'   Name of the parameter to be mapped to the x-axis.
#' @param y (`character(1)`)
#'   Name of the parameter to be mapped to the y-axis.
#' @template theme
#' 
#' @export
marginal_plot = function(archive, x = NULL, y = NULL, theme = ggplot2::theme_minimal()) {
  param_ids = archive$search_space$data$id
  assert_choice(x, param_ids)
  assert_choice(y, param_ids)

  # there should only be one objective, e.g. `classif.ce`
  cost = archive$codomain$data$id[[1]]

  data = na.omit(archive$data, cols = c(x, y))

  .data = NULL
  g = ggplot2::ggplot(data = data, ggplot2::aes(
      x = .data[[x]],
      y = .data[[y]],
      fill = .data[[cost]]
    )) +
    ggplot2::geom_point() +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(
      title = "2D marginal plot"
    ) +
    theme

  # log-scale params
  if (archive$search_space$is_logscale[[x]]) {
    g = g + ggplot2::scale_x_log10()
  }
  if (archive$search_space$is_logscale[[y]]) {
    g = g + ggplot2::scale_y_log10()
  }

  return(g)
}
