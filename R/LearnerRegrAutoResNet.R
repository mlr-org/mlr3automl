#' @title Regression ResNet Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @return Object of class [R6::R6Class] and `LearnerRegrAutoResNet`.
#'
#' @templateVar id regr.auto_resnet
#' @template example_learner
#'
#' @export
LearnerRegrAutoResNet = R6Class(
  "LearnerRegrAutoResNet",
  inherit = LearnerRegrAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_resnet", rush = NULL) {
      super$initialize(id = id, learner_ids = "resnet", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["regr.auto_resnet"]] = LearnerRegrAutoResNet
