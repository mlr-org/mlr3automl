#' @title Regression SVM Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @return Object of class [R6::R6Class] and `LearnerRegrAutoSVM`.
#'
#' @templateVar id regr.auto_svm
#' @template example_learner
#'
#' @export
LearnerRegrAutoSVM = R6Class(
  "LearnerRegrAutoSVM",
  inherit = LearnerRegrAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_svm", rush = NULL) {
      super$initialize(id = id, learner_ids = "svm", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["regr.auto_svm"]] = LearnerRegrAutoSVM
