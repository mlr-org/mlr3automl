#' @title Classification LightGBM Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#'
#' @export
LearnerClassifAutoLightGBM = R6Class("LearnerClassifAutoLightGBM",
  inherit = LearnerClassifAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_lightgbm") {
      super$initialize(id = id, learner_ids = "lightgbm")
    }
  )
)

#' @include aaa.R
learners[["classif.auto_lightgbm"]] = LearnerClassifAutoLightGBM
