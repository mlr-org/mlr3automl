#' @title Lda Auto
#'
#' @include mlr_auto.R
#'
#' @description
#' Lda auto.
#'
#' @template param_id
#' @template param_task
#' @template param_measure
#' @template param_task
#' @template param_measure
#' @template param_size
#' @template param_n_threads
#' @template param_timeout
#'
#' @export
AutoLda = R6Class("AutoLda",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "lda") {
      super$initialize(
        id = id,
        task_types = "classif",
        properties = "hyperparameter-free",
        packages = c("mlr3", "mlr3learners", "MASS"),
        devices = "cpu")
    },

    #' @description
    #' Create the graph for the auto.
    graph = function(task, measure, n_threads, timeout) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)

      require_namespaces("mlr3learners")

      learner = lrn("classif.lda", id = "lda")

      po("removeconstants", id = "lda_removeconstants") %>>%
        po("imputehist", id = "lda_imputehist") %>>%
        po("imputeoor", id = "lda_imputeoor") %>>%
        po("fixfactors", id = "lda_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "lda_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "lda_collapse") %>>%
        po("removeconstants", id = "lda_post_removeconstants") %>>%
        learner
    }
  )
)

mlr_auto$add("lda", function() AutoLda$new())


