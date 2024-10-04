#' @title Classification Neural Network Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#'
#' @export
LearnerClassifAutoNnet = R6Class("LearnerClassifAutoNnet",
  inherit = LearnerClassifAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_nnet") {
      super$initialize(id = id, learner_ids = "nnet")
    }
  )
)

#' @include aaa.R
learners[["classif.auto_nnet"]] = LearnerClassifAutoNnet
