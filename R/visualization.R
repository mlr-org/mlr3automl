#' @title Cost-Over-Time Plot
#' 
#' @description Plots cost over time using [ggplot2].
#' 
#' @template param_instance
#' @template param_theme
#' 
#' @export
cost_over_time = function(instance, x = "config_id", theme = ggplot2::theme_minimal(), ...) {
  # there should be only a single objective, e.g. `classif.ce`
  cost = instance$objective$codomain$data$id[[1]]
  
  .data = NULL
  ggplot2::ggplot(data = instance$archive$data, ggplot2::aes(
      x = seq_len(nrow(instance$archive$data)),
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
