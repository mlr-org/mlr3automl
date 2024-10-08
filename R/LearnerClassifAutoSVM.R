#' @title Classification SVM Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#'
#' @export
LearnerClassifAutoSVM = R6Class("LearnerClassifAutoSVM",
  inherit = LearnerClassifAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_svm") {
      super$initialize(id = id, learner_ids = "svm")
    }
  )
)

#' @include aaa.R
learners[["classif.auto_svm"]] = LearnerClassifAutoSVM
