#' @title Classification FT-Transformer Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#'
#' @export
LearnerClassifAutoFTTransformer = R6Class("LearnerClassifAutoFTTransformer",
  inherit = LearnerClassifAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_ft_transformer") {
      super$initialize(id = id, learner_ids = "ft_transformer")
    }
  )
)

#' @include aaa.R
learners[["classif.auto_ft_transformer"]] = LearnerClassifAutoFTTransformer


