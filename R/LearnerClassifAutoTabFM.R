#' @title Classification TabFM Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @return Object of class [R6::R6Class] and `LearnerClassifAutoTabFM`.
#'
#' @templateVar id classif.auto_tabfm
#' @templateVar packages c("mlr3extralearners", "callr")
#' @template example_learner
#'
#' @export
LearnerClassifAutoTabFM = R6Class(
  "LearnerClassifAutoTabFM",
  inherit = LearnerClassifAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_tabfm", rush = NULL) {
      super$initialize(id = id, learner_ids = "tabfm", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["classif.auto_tabfm"]] = LearnerClassifAutoTabFM
