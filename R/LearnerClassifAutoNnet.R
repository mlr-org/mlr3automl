#' @title Classification Neural Network Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#'
#' @export
LearnerClassifAutoNnet = R6Class("LearnerClassifAutoNnet",
  inherit = LearnerClassifAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_nnet") {
      super$initialize(id = id)

      # reduce parameter set to the relevant parameters
      private$.param_set = private$.param_set$subset(
        c("learner_ids",
        "learner_timeout",
        "small_data_size",
        "small_data_resampling",
        "max_cardinality",
        "resampling",
        "terminator",
        "measure",
        "lhs_size",
        "callbacks",
        "store_benchmark_result")
      )

      self$param_set$set_values(learner_ids = "nnet")
      self$packages = c("mlr3tuning", "mlr3learners", "mlr3pipelines", "mlr3mbo", "mlr3automl", "nnet")
    }
  )
)

#' @include aaa.R
learners[["classif.auto_nnet"]] = LearnerClassifAutoNnet
