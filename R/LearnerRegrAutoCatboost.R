#' @title Regression Gradient Boosted Decision Trees Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#'
#' @export
LearnerRegrAutoCatboost = R6Class("LearnerRegrAutoCatboost",
  inherit = LearnerRegrAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_catboost") {
      super$initialize(id = id, learner_ids = "catboost")
    }
  )
)

#' @include aaa.R
learners[["regr.auto_catboost"]] = LearnerRegrAutoCatboost


