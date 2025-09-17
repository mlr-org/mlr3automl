#' @title Svm Auto
#'
#' @description
#' Svm auto.
#'
#' @template param_id
#'
#' @include mlr_auto.R
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
    },

    #' @description
    #' Create the graph for the auto.
    #'
    #' @param task ([mlr3::Task]).
    #' @param measure ([mlr3::Measure]).
    #' @param n_threads (`numeric(1)`).
    #' @param timeout (`numeric(1)`).
    graph = function(task, measure, n_threads, timeout) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)

      svm_type = if (task$task_type == "classif") "C-classification" else "eps-regression"
      branch_svm = po("removeconstants", id = "svm_removeconstants") %>>%
        po("imputehist", id = "svm_imputehist") %>>%
        po("imputeoor", id = "svm_imputeoor") %>>%
        po("fixfactors", id = "svm_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "svm_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "svm_collapse") %>>%
        po("encode", method = "one-hot", id = "svm_encode") %>>%
        po("removeconstants", id = "svm_post_removeconstants") %>>%
        lrn(sprintf("%s.svm", task$task_type), id = "svm", type = svm_type)
    },

    #' @description
    #' Get the default values for the auto.
    #'
    #' @param task ([mlr3::Task]).
    default_values = function(task) {
      list(
        svm.cost = log(1),
        svm.kernel = "radial",
        svm.degree = NA_integer_,
        svm.gamma = log(1 / length(task$feature_names))
      )
    }
  ),

  active = list(

    #' @field search_space (`ParamSet`).
    search_space = function() {
      ps(
        svm.cost    = p_dbl(1e-4, 1e4, logscale = TRUE),
        svm.kernel  = p_fct(c("polynomial", "radial", "sigmoid", "linear")),
        svm.degree  = p_int(2, 5, depends =  quote(svm.kernel == "polynomial")),
        svm.gamma   = p_dbl(1e-4, 1e4, logscale = TRUE, depends = quote(svm.kernel %in% c("polynomial", "sigmoid", "radial")))
      )
    }
  )
)

mlr_auto$add("svm", function() AutoSvm$new())
