#' @title ResNet Auto
#'
#' @include mlr_auto.R
#'
#' @description
#' ResNet auto.
#'
#' @template param_id
#' @template param_task
#' @template param_measure
#' @template param_n_threads
#' @template param_timeout
#'
#' @export
AutoResNet = R6Class("AutoResNet",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "resnet") {
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

      learner = lrn(sprintf("%s.tab_resnet", task$task_type),
       id = "resnet",
       measures_valid = measure,
       patience = self$early_stopping_rounds(task),
       batch_size = 32L
      )
      set_threads(learner, n_threads)

      po("removeconstants", id = "resnet_removeconstants") %>>%
        po("imputeoor", id = "resnet_imputeoor") %>>%
        po("fixfactors", id = "resnet_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "resnet_imputesample") %>>%
        po("encodeimpact", id = "resnet_encode") %>>%
        po("removeconstants", id = "resnet_post_removeconstants") %>>%
        learner
    },

    #' @description
    #' Estimate the memory for the auto.
    estimate_memory = function(task) {
      memory_size = task$nrow * task$ncol * 8 * 10 / 1e6
      lg$info("ResNet memory size: %s MB", round(memory_size))
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
        resnet.n_blocks            = p_int(1, 16),
        resnet.d_block             = p_int(64, 1024),
        resnet.d_hidden_multiplier = p_int(1, 4),
        resnet.dropout1            = p_dbl(0, 0.5),
        resnet.dropout2            = p_dbl(0, 0.5),
        resnet.opt.lr              = p_dbl(1e-5, 1e-2, logscale = TRUE),
        resnet.opt.weight_decay    = p_dbl(1e-6, 1e-3, logscale = TRUE),
        resnet.epochs              = p_int(1L, 100L, tags = "internal_tuning", aggr = function(x) as.integer(ceiling(mean(unlist(x)))))
      )


    }
  )
)

mlr_auto$add("resnet", function() AutoResNet$new())
