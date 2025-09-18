#' @title Classification Fastai Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#'
#' @export
LearnerClassifAutoFastai = R6Class("LearnerClassifAutoFastai",
  inherit = LearnerClassifAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_fastai") {
      super$initialize(id = id, learner_ids = "fastai")
    }
  )
)

#' @include aaa.R
learners[["classif.auto_fastai"]] = LearnerClassifAutoFastai
