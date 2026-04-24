#' @title Sugar Function for Auto Objects
#'
#' @description
#' Sugar function to retrieve [Auto] objects from [mlr_auto].
#'
#' @param .key (`character(1)`)\cr
#'   Key of the object to retrieve.
#'   If missing, the dictionary itself is returned.
#' @param ... (named `list()`)\cr
#'   Additional arguments passed to the constructor.
#'
#' @return [Auto]
#' @export
#' @rdname mlr_auto
#' @examples
#' auto("catboost")
auto = function(.key, ...) {
  dictionary_sugar_get(mlr_auto, .key, ...)
}
