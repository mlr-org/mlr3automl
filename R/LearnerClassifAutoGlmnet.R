#' @title Classification GLM with Elastic Net Regularization Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#'
#' @export
LearnerClassifAutoGlmnet = R6Class("LearnerClassifAutoGlmnet",
  inherit = LearnerClassifAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_glmnet") {
      super$initialize(id = id, learner_ids = "glmnet")
    }
  )
)

#' @include aaa.R
learners[["classif.auto_glmnet"]] = LearnerClassifAutoGlmnet
