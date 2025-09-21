#' @title Mlp Auto
#'
#' @include mlr_auto.R
#'
#' @description
#' Mlp auto.
#'
#' @template param_id
#' @template param_task
#' @template param_measure
#' @template param_n_threads
#' @template param_timeout
#'
#' @export
AutoMlp = R6Class("AutoMlp",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "mlp") {
      super$initialize(id = id)
      self$task_types = c("classif", "regr")
      self$properties = "internal_tuning"
      self$packages = c("mlr3", "mlr3torch")
    },

    #' @description
    #' Create the graph for the auto.
    graph = function(task, measure, n_threads, timeout) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)

      require_namespaces("mlr3torch")

      learner = lrn(sprintf("%s.mlp", task$task_type),
       id = "mlp",
       measures_valid = measure,
       patience = self$early_stopping_rounds(task),
       batch_size = 32L
      )
      set_threads(learner, n_threads)

      po("removeconstants", id = "mlp_removeconstants") %>>%
        po("imputeoor", id = "mlp_imputeoor") %>>%
        po("fixfactors", id = "mlp_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "mlp_imputesample") %>>%
        po("encodeimpact", id = "mlp_encode") %>>%
        po("removeconstants", id = "mlp_post_removeconstants") %>>%
        learner
    },

    #' @description
    #' Estimate the memory for the auto.
    estimate_memory = function(task) {
      memory_size = task$nrow * task$ncol * 8 * 10 / 1e6
      lg$info("Mlp memory size: %s MB", round(memory_size))
      memory_size
    },

    #' @description
    #' Get the default values for the auto.
    default_values = function(task) {
      list()
    }
  ),

  active = list(

    #' @field search_space ([paradox::ParamSet]).
    search_space = function(rhs) {
      assert_ro_binding(rhs)
      ps(
        mlp.n_layers              = p_int(1L, 16L),
        mlp.neurons               = p_int(1L, 1024L),
        mlp.p                     = p_dbl(0, 0.5),
        mlp.opt.lr                = p_dbl(1e-5, 1e-2, logscale = TRUE),
        mlp.opt.weight_decay      = p_dbl(1e-6, 1e-3, logscale = TRUE),
        mlp.epochs                = p_int(1L, 100L, tags = "internal_tuning", aggr = function(x) as.integer(ceiling(mean(unlist(x)))))
      )
    }
  )
)

mlr_auto$add("mlp", function() AutoMlp$new())
