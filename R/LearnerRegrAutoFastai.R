#' @title Regression Fastai Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#'
#' @export
LearnerRegrAutoFastai = R6Class("LearnerRegrAutoFastai",
  inherit = LearnerRegrAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_fastai") {
      super$initialize(id = id, learner_ids = "fastai")
    }
  )
)

#' @include aaa.R
learners[["regr.auto_fastai"]] = LearnerRegrAutoFastai


