#' @title Classification Gradient Boosted Decision Trees Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#'
#' @export
LearnerClassifAutoCatboost = R6Class("LearnerClassifAutoCatboost",
  inherit = LearnerClassifAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_catboost") {
      super$initialize(id = id, learner_ids = "catboost")
    }
  )
)

#' @include aaa.R
learners[["classif.auto_catboost"]] = LearnerClassifAutoCatboost
