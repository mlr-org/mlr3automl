#' @title Classification Gradient Boosted Decision Trees Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @return Object of class [R6::R6Class] and `LearnerClassifAutoCatboost`.
#'
#' @templateVar id classif.auto_catboost
#' @template example_learner
#'
#' @export
LearnerClassifAutoCatboost = R6Class(
  "LearnerClassifAutoCatboost",
  inherit = LearnerClassifAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_catboost", rush = NULL) {
      super$initialize(id = id, learner_ids = "catboost", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["classif.auto_catboost"]] = LearnerClassifAutoCatboost
