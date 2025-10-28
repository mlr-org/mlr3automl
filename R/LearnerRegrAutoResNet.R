#' @title Regression ResNet Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#'
#' @export
LearnerRegrAutoResNet = R6Class("LearnerRegrAutoResNet",
  inherit = LearnerRegrAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_resnet") {
      super$initialize(id = id, learner_ids = "resnet")
    }
  )
)

#' @include aaa.R
learners[["regr.auto_resnet"]] = LearnerRegrAutoResNet




