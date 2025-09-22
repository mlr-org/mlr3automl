#' @title Extra Trees Auto
#'
#' @include mlr_auto.R
#'
#' @description
#' Extra Trees auto.
#'
#' @template param_id
#' @template param_n_threads
#' @template param_timeout
#' @template param_task
#' @template param_measure
#' @template param_size
#'
#' @export
AutoExtraTrees = R6Class("AutoExtraTrees",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "extra_trees") {
      super$initialize(id = id)
      self$task_types = c("classif", "regr")
      self$properties = c("large_data_sets", "hyperparameter-free")
      self$packages = c("mlr3", "mlr3learners", "ranger")
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

      require_namespaces("mlr3learners")

      learner = lrn(sprintf("%s.ranger", task$task_type),
        id = "extra_trees",
        splitrule = "extratrees",
        num.trees = 100L,
        replace = FALSE,
        sample.fraction = 1)
      set_threads(learner, n_threads)

      po("removeconstants", id = "extra_trees_removeconstants") %>>%
        po("imputeoor", id = "extra_trees_imputeoor") %>>%
        po("fixfactors", id = "extra_trees_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "extra_trees_imputesample") %>>%
        po("collapsefactors", target_level_count = 40, id = "extra_trees_collapse") %>>%
        po("removeconstants", id = "extra_trees_post_removeconstants") %>>%
        learner
    },

    #' @description
    #' Estimate the memory for the auto.
    #'
    #' @param task ([mlr3::Task]).
    estimate_memory = function(task) {
      num_trees = 100
      tree_size_bytes = task$nrow / 60000 * 1e6
      ceiling((tree_size_bytes * num_trees) / 1e6)
    }
  )
)

mlr_auto$add("extra_trees", function() AutoExtraTrees$new())


