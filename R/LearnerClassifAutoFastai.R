#' @title Classification Fastai Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @return Object of class [R6::R6Class] and `LearnerClassifAutoFastai`.
#'
#' @templateVar id classif.auto_fastai
#' @template example_learner
#'
#' @export
LearnerClassifAutoFastai = R6Class(
  "LearnerClassifAutoFastai",
  inherit = LearnerClassifAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_fastai", rush = NULL) {
      super$initialize(id = id, learner_ids = "fastai", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["classif.auto_fastai"]] = LearnerClassifAutoFastai
