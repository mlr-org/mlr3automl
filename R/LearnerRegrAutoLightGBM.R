#' @title Regression LightGBM Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @return Object of class [R6::R6Class] and `LearnerRegrAutoLightGBM`.
#'
#' @templateVar id regr.auto_lightgbm
#' @template example_learner
#'
#' @export
LearnerRegrAutoLightGBM = R6Class(
  "LearnerRegrAutoLightGBM",
  inherit = LearnerRegrAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_lightgbm", rush = NULL) {
      super$initialize(id = id, learner_ids = "lightgbm", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["regr.auto_lightgbm"]] = LearnerRegrAutoLightGBM
