#' @title Dictionary of Auto Objects
#'
#' @description
#' A dictionary of [Auto] objects.
#'
#' @export
mlr_auto = R6Class("DictionaryAuto",
  inherit = Dictionary,
  cloneable = FALSE,
)$new()
