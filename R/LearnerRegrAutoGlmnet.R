#' @title Regression GLM with Elastic Net Regularization Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @return Object of class [R6::R6Class] and `LearnerRegrAutoGlmnet`.
#'
#' @templateVar id regr.auto_glmnet
#' @template example_learner
#'
#' @export
LearnerRegrAutoGlmnet = R6Class(
  "LearnerRegrAutoGlmnet",
  inherit = LearnerRegrAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_glmnet", rush = NULL) {
      super$initialize(id = id, learner_ids = "glmnet", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["regr.auto_glmnet"]] = LearnerRegrAutoGlmnet
