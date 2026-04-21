#' @title Classification AutoML Learner
#'
#' @description
#' The [LearnerClassifAuto] is an automated machine learning (AutoML) system for classification tasks.
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
LearnerClassifAuto = R6Class(
  "LearnerClassifAuto",
  inherit = LearnerAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "classif.auto",
      learner_ids,
      rush = NULL
    ) {
      super$initialize(
        id = id,
        learner_ids = learner_ids,
        task_type = "classif",
        predict_types = c("response", "prob"),
        properties = c("missings", "weights", "twoclass", "multiclass"),
        rush = rush
      )
    }
  )
)

#' @include aaa.R
learners[["classif.auto"]] = LearnerClassifAuto
