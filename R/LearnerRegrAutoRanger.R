#' @title Regression Ranger Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#'
#' @export
LearnerRegrAutoRanger = R6Class("LearnerRegrAutoRanger",
  inherit = LearnerRegrAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_ranger") {
      super$initialize(id = id, learner_ids = "ranger")
    }
  )
)

#' @include aaa.R
learners[["regr.auto_ranger"]] = LearnerRegrAutoRanger
