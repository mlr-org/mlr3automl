#' @title Classification LightGBM Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @export
LearnerClassifAutoLightGBM = R6Class(
  "LearnerClassifAutoLightGBM",
  inherit = LearnerClassifAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_lightgbm", rush = NULL) {
      super$initialize(id = id, learner_ids = "lightgbm", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["classif.auto_lightgbm"]] = LearnerClassifAutoLightGBM
