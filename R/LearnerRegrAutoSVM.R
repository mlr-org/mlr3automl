#' @title Regression SVM Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @template param_id
#'
#' @export
LearnerRegrAutoSVM = R6Class("LearnerRegrAutoSVM",
  inherit = LearnerRegrAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "regr.auto_svm") {
      super$initialize(id = id, learner_ids = "svm")
    }
  )
)

#' @include aaa.R
learners[["regr.auto_svm"]] = LearnerRegrAutoSVM
