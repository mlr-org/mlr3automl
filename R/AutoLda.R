#' @include mlr_auto.R
#' @export
AutoLda = R6Class("AutoLda",
  inherit = Auto,
  public = list(
    initialize = function(id = "lda") {
      super$initialize(id = id)
      self$task_types = "classif"
      self$properties = "hyperparameter-free"
    },

    graph = function(task, measure, n_threads, timeout) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)

      learner = lrn("classif.lda", id = "lda")

      po("removeconstants", id = "lda_removeconstants") %>>%
        po("imputehist", id = "lda_imputehist") %>>%
        po("imputeoor", id = "lda_imputeoor") %>>%
        po("fixfactors", id = "lda_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "lda_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "lda_collapse") %>>%
        po("removeconstants", id = "lda_post_removeconstants") %>>%
        learner
    },

    default_values = function(task) {
      list()
    }
  ),

  active = list(
    search_space = function() {
      ps()
    }
  )
)

mlr_auto$add("lda", function() AutoLda$new())


