#' @title Svm Auto
#'
#' @include mlr_auto.R
#'
#' @description
#' Svm auto.
#'
#' @template param_id
#' @template param_task
#' @template param_measure
#' @template param_n_threads
#' @template param_timeout
#'
#' @export
AutoSvm = R6Class("AutoSvm",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "svm") {
      super$initialize(id = id)
      self$task_types = c("classif", "regr")
      self$properties = character(0)
      self$packages = c("mlr3", "mlr3learners", "e1071")
    },

    #' @description
    #' Create the graph for the auto.
    graph = function(task, measure, n_threads, timeout) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)

      require_namespaces("mlr3learners")

      svm_type = if (task$task_type == "classif") "C-classification" else "eps-regression"
      branch_svm = po("removeconstants", id = "svm_removeconstants") %>>%
        po("imputehist", id = "svm_imputehist") %>>%
        po("imputeoor", id = "svm_imputeoor") %>>%
        po("fixfactors", id = "svm_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "svm_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "svm_collapse") %>>%
        po("encode", method = "one-hot", id = "svm_encode") %>>%
        po("removeconstants", id = "svm_post_removeconstants") %>>%
        lrn(sprintf("%s.svm", task$task_type), id = "svm", type = svm_type, kernel = "radial")
    },

    #' @description
    #' Get the default values for the auto.
    default_values = function(task) {
      list(
        svm.cost = log(1),
        svm.gamma = log(1 / length(task$feature_names))
      )
    }
  ),

  private = list(
    .search_space = ps(
        svm.cost    = p_dbl(1e-4, 1e4, logscale = TRUE),
        svm.gamma   = p_dbl(1e-4, 1e4, logscale = TRUE)
      )
  )
)

mlr_auto$add("svm", function() AutoSvm$new())
