#' @title Classification Ranger Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#'
#' @export
LearnerClassifAutoRanger = R6Class("LearnerClassifAutoRanger",
  inherit = LearnerClassifAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_ranger", rush = NULL) {
      super$initialize(id = id, learner_ids = "ranger", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["classif.auto_ranger"]] = LearnerClassifAutoRanger
