#' @title Regression Gradient Boosted Decision Trees Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @return Object of class [R6::R6Class] and `LearnerRegrAutoCatboost`.
#'
#' @templateVar id regr.auto_catboost
#' @template example_learner
#'
#' @export
LearnerRegrAutoCatboost = R6Class(
  "LearnerRegrAutoCatboost",
  inherit = LearnerRegrAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_catboost", rush = NULL) {
      super$initialize(id = id, learner_ids = "catboost", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["regr.auto_catboost"]] = LearnerRegrAutoCatboost
