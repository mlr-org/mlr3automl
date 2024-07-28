#' @title Classification Extra Trees Auto Learner
#'
#' @description
#' Classification auto learner.
#'
#' @template param_id
#'
#' @export
LearnerClassifAutoExtraTrees = R6Class("LearnerClassifExtraTrees",
  inherit = LearnerClassifAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto_extra_trees") {

      super$initialize(id = id)

      # reduce parameter set to the relevant parameters
      private$.param_set = private$.param_set$subset(
        c("learner_ids",
          "learner_timeout",
          "max_nthread",
          "max_memory",
          "large_data_size",
          "large_data_learner_ids",
          "large_data_nthread",
          "small_data_size",
          "small_data_resampling",
          "max_cardinality",
          "extra_trees_max_cardinality",
          "resampling",
          "terminator",
          "measure",
          "lhs_size",
          "callbacks",
          "store_benchmark_result"))

      self$param_set$set_values(learner_ids = "extra_trees")
      self$packages = c("mlr3tuning", "mlr3learners", "mlr3pipelines", "mlr3mbo", "mlr3automl", "ranger")
    }
  )
)

#' @include aaa.R
learners[["classif.auto_extra_trees"]] = LearnerClassifAutoExtraTrees
