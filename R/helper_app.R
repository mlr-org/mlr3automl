#' @title Custom conditionalPanel for hyperparameter selection
#' 
#' @description
#' Used for Marginal Plots and Partial Dependence Plots.
#' 
#' @param condition (`character(1)`)\cr
#'   Passed to the `condition` argument of `[shiny::conditionalPanel]`.
#' @param prefix (`character(1)`)\cr
#'   Prefix of input slot names.
#' @param learner_ids (`character()`)\cr
#'   Vector of all possible learner/branch IDs.
#' @param param_ids (`character()`)\cr
#'   Vector of all possible param IDs.
#' @param ... (anything)
#'   Additional arguments passed to `[shiny::conditionalPanel]`.
#' 
param_panel = function(condition, prefix, learner_ids, param_ids, ...) {
  assert_string(condition)
  assert_string(prefix)
  assert_character(learner_ids)
  assert_character(param_ids)

  shiny::conditionalPanel(
    condition,
    shiny::selectInput(paste0(prefix, "_branch"),
      label = "Select branch:",
      choices = learner_ids
    ),
    # choices and selected are just placeholders for initialization
    shiny::selectInput(paste0(prefix, "_x"),
      label = "Select x-axis:",
      choices = param_ids,
      selected = param_ids[[1]]
    ),
    shiny::selectInput(paste0(prefix, "_y"),
      label = "Select y-axis:",
      choices = param_ids,
      selected = param_ids[[2]]
    ),
    ...
  )
}
