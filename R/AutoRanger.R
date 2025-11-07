#' @title Ranger Auto
#'
#' @include mlr_auto.R Auto.R
#'
#' @description
#' Ranger auto.
#'
#' @template param_id
#' @template param_task
#' @template param_measure
#' @template param_n_threads
#' @template param_timeout
#' @template param_memory_limit
#' @template param_large_data_set
#' @template param_size
#' @template param_devices
#' @template param_pv
#' @template param_graph
#'
#' @export
AutoRanger = R6Class("AutoRanger",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "ranger") {
      super$initialize(id = id,
        properties = "large_data_sets",
        task_types = c("classif", "regr"),
        packages = c("mlr3", "mlr3learners", "ranger"),
        devices = "cpu"
      )
    },

    #' @description
    #' Create the graph for the auto.
    graph = function(task, measure, n_threads, timeout, devices, pv) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)

      require_namespaces("mlr3learners")

      learner = lrn(sprintf("%s.ranger", task$task_type), id = "ranger")
      set_threads(learner, n_threads)

      graph = po("removeconstants", id = "ranger_removeconstants") %>>%
        po("imputeoor", id = "ranger_imputeoor") %>>%
        po("fixfactors", id = "ranger_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "ranger_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "ranger_collapse") %>>%
        po("removeconstants", id = "ranger_post_removeconstants") %>>%
        learner

      if (task$nrow * task$ncol > pv$large_data_size) {
        graph = po("subsample", frac = 0.25, stratify = inherits(task, "TaskClassif"), use_groups = FALSE, id = "ranger_subsample") %>>% graph
      }

      graph
    },

    #' @description
    #' Estimate the memory for the auto.
    estimate_memory = function(task) {
      upper = self$search_space(task)$upper

      num_trees = upper["ranger.num.trees"]
      tree_size = task$nrow / 60000 * 1e6

      memory_size = (tree_size * num_trees) / 1e6
      lg$info("Ranger memory size: %s MB", round(memory_size))
      ceiling(memory_size)
    },

    #' @description
    #' Modify the graph for the final model.
    final_graph = function(graph, task, pv) {
      if (task$nrow * task$ncol > pv$large_data_size) {
        graph$param_set$set_values(ranger_subsample.frac = 1)
      }
    }
  ),

  private = list(
    .search_space = ps(
        ranger.mtry.ratio      = p_dbl(0, 1),
        ranger.replace         = p_lgl(),
        ranger.sample.fraction = p_dbl(1e-1, 1),
        ranger.num.trees       = p_int(500L, 2000L)
    ),

    .default_values = list(
      ranger.mtry.ratio = 0.5,
      ranger.replace = TRUE,
      ranger.sample.fraction = 0.632,
      ranger.num.trees = 1000L
    )
  )
)

mlr_auto$add("ranger", function() AutoRanger$new())


