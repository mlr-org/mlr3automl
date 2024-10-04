#' @title Regression LightGBM Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#'
#' @export
LearnerRegrAutoLightGBM = R6Class("LearnerRegrAutoLightGBM",
  inherit = LearnerRegrAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_lightgbm") {
      super$initialize(id = id, learner_ids = "lightgbm")
    }
  )
)

#' @include aaa.R
learners[["regr.auto_lightgbm"]] = LearnerRegrAutoLightGBM
