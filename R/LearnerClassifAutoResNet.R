#' @title Classification ResNet Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#'
#' @export
LearnerClassifAutoResNet = R6Class("LearnerClassifAutoResNet",
  inherit = LearnerClassifAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_resnet", rush = NULL) {
      super$initialize(id = id, learner_ids = "resnet", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["classif.auto_resnet"]] = LearnerClassifAutoResNet


