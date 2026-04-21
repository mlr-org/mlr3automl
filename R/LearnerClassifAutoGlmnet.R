#' @title Classification GLM with Elastic Net Regularization Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @export
LearnerClassifAutoGlmnet = R6Class(
  "LearnerClassifAutoGlmnet",
  inherit = LearnerClassifAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_glmnet", rush = NULL) {
      super$initialize(id = id, learner_ids = "glmnet", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["classif.auto_glmnet"]] = LearnerClassifAutoGlmnet
