#' @title Regression Ranger Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @return Object of class [R6::R6Class] and `LearnerRegrAutoRanger`.
#'
#' @templateVar id regr.auto_ranger
#' @template example_learner
#'
#' @export
LearnerRegrAutoRanger = R6Class(
  "LearnerRegrAutoRanger",
  inherit = LearnerRegrAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_ranger", rush = NULL) {
      super$initialize(id = id, learner_ids = "ranger", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["regr.auto_ranger"]] = LearnerRegrAutoRanger
