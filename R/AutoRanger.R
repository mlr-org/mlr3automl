#' @include mlr_auto.R
#' @export
AutoRanger = R6Class("AutoRanger",
  inherit = Auto,
  public = list(
    initialize = function(id = "ranger") {
      super$initialize(id = id)
      self$task_types = c("classif", "regr")
      self$properties = "large_data_sets"
    },

    graph = function(task, measure, n_threads, timeout) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)

      learner = lrn(sprintf("%s.ranger", task$task_type), id = "ranger")
      set_threads(learner, n_threads)

      po("removeconstants", id = "ranger_removeconstants") %>>%
        po("imputeoor", id = "ranger_imputeoor") %>>%
        po("fixfactors", id = "ranger_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "ranger_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "ranger_collapse") %>>%
        po("removeconstants", id = "ranger_post_removeconstants") %>>%
        learner
    },

    estimate_memory = function(task) {
      upper = self$search_space$upper

      num_trees = upper["ranger.num.trees"]
      tree_size = task$nrow / 60000 * 1e6

      memory_size = (tree_size * num_trees) / 1e6
      lg$info("Ranger memory size: %s MB", round(memory_size))
      memory_size
    },

    default_values = function(task) {
      list(
        ranger.mtry.ratio = 0.5,
        ranger.replace = TRUE,
        ranger.sample.fraction = 0.632,
        ranger.num.trees = 1000L
      )
    }
  ),

  active = list(
    search_space = function() {
      ps(
        ranger.mtry.ratio      = p_dbl(0, 1),
        ranger.replace         = p_lgl(),
        ranger.sample.fraction = p_dbl(1e-1, 1),
        ranger.num.trees       = p_int(500L, 2000L)
      )
    }
  )
)

mlr_auto$add("ranger", function() AutoRanger$new())


