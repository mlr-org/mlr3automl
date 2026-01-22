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
#' @template param_devices
#'
#' @export
AutoMlp = R6Class("AutoMlp",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "mlp") {
      super$initialize(
        id = id,
        properties = "internal_tuning",
        task_types = c("classif", "regr"),
        packages = c("mlr3", "mlr3torch"),
        devices = "cuda"
      )
    },

    #' @description
    #' Create the graph for the auto.
    graph = function(task, measure, n_threads, timeout, devices) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)
      assert_subset(devices, c("cuda", "cpu"))

      require_namespaces("mlr3torch")

      device = if ("cuda" %in% devices) "cuda" else "auto"

      learner = lrn(sprintf("%s.mlp", task$task_type),
       id = "mlp",
       measures_valid = measure,
       patience = self$early_stopping_rounds(task),
       batch_size = 32L,
       device = device
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
      nrow = task$nrow
      n_layers = private$.search_space$upper[["mlp.n_layers"]]
      neurons = private$.search_space$upper[["mlp.neurons"]]
      
      baseline = 6.43
      b_nrow = 2e-07
      b_n_layers =  0.0053
      b_neurons =  0.003
      
      memory_size = exp(baseline + (nrow * b_nrow) + (n_layers * b_n_layers) + (neurons * b_neurons)) # gamma model
      memory_size = memory_size * 1.3  # scale by 30% to over-predict memory size in most cases
      lg$info("Mlp memory size: %s MB", round(memory_size))
      ceiling(memory_size)
    }
  ),

  private = list(
    .search_space = ps(
        mlp.n_layers              = p_int(1L, 16L),
        mlp.neurons               = p_int(1L, 1024L),
        mlp.p                     = p_dbl(0, 0.5),
        mlp.opt.lr                = p_dbl(1e-5, 1e-2, logscale = TRUE),
        mlp.opt.weight_decay      = p_dbl(1e-6, 1e-3, logscale = TRUE),
        mlp.epochs                = p_int(1L, 100L, tags = "internal_tuning", aggr = function(x) as.integer(ceiling(mean(unlist(x)))))
    ),

    .default_values = list(
      mlp.n_layers = 4L,
      mlp.neurons = 128L,
      mlp.p = 0.2,
      mlp.opt.lr = 1e-5,
      mlp.opt.weight_decay = 1e-6
    )
  )
)

mlr_auto$add("mlp", function() AutoMlp$new())
