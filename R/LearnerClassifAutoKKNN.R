#' @title Classification k-Nearest-Neighbor Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @export
LearnerClassifAutoKKNN = R6Class(
  "LearnerClassifAutoKKNN",
  inherit = LearnerClassifAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_kknn", rush = NULL) {
      super$initialize(id = id, learner_ids = "kknn", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["classif.auto_kknn"]] = LearnerClassifAutoKKNN
