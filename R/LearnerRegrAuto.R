#' @title Regression AutoML Learner
#'
#' @description
#' The [LearnerRegrAuto] is an automated machine learning (AutoML) system for regression tasks.
#' It combines preprocessing, a switch between multiple learners,
#' and hyperparameter tuning to find the best model for the given task.
#'
#' @template param_id
#' @template param_learner_ids
#' @template param_rush
#' @template section_debugging
#' @template section_parameters
#'
#' @export
#' @include LearnerAuto.R
LearnerRegrAuto = R6Class(
  "LearnerRegrAuto",
  inherit = LearnerAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "regr.auto",
      learner_ids,
      rush = NULL
    ) {
      super$initialize(
        id = id,
        learner_ids = learner_ids,
        task_type = "regr",
        predict_types = "response",
        properties = c("missings", "weights"),
        rush = rush
      )
    }
  )
)

#' @include aaa.R
learners[["regr.auto"]] = LearnerRegrAuto
