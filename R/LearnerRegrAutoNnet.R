#' @title Regression Neural Network Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#'
#' @export
LearnerRegrAutoNnet = R6Class("LearnerRegrAutoNnet",
  inherit = LearnerRegrAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_nnet") {
      super$initialize(id = id, learner_ids = "nnet")
    }
  )
)

#' @include aaa.R
learners[["regr.auto_nnet"]] = LearnerRegrAutoNnet
