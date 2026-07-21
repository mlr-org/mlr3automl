#' @title Classification FT-Transformer Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#' @template param_rush
#'
#' @return Object of class [R6::R6Class] and `LearnerClassifAutoFTTransformer`.
#'
#' @templateVar id classif.auto_ft_transformer
#' @template example_learner
#'
#' @export
# nolint next: object_length_linter
LearnerClassifAutoFTTransformer = R6Class(
  "LearnerClassifAutoFTTransformer",
  inherit = LearnerClassifAuto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_ft_transformer", rush = NULL) {
      super$initialize(id = id, learner_ids = "ft_transformer", rush = rush)
    }
  )
)

#' @include aaa.R
learners[["classif.auto_ft_transformer"]] = LearnerClassifAutoFTTransformer
