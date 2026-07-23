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
#' @return Object of class [R6::R6Class] and `AutoMlp`.
#'
#' @templateVar id mlp
#' @template example_auto
#'
#' @export
AutoMlp = R6Class(
  "AutoMlp",
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
        devices = c("cuda", "cpu")
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

      # construct directly instead of via lrn() because mlr3extralearners also
      # registers the key "classif.mlp" and overwrites the mlr3torch learner
      learner = mlr3torch::LearnerTorchMLP$new(task_type = task$task_type)
      learner$id = "mlp"
      learner$param_set$set_values(
        measures_valid = measure,
        patience = self$early_stopping_rounds(task, budget = self$search_space(task)$upper[["mlp.epochs"]]),
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

      # coefficients of the gamma model fitted in memory_experiments/Mlp
      baseline = 6.43
      b_nrow = 2e-07
      b_n_layers = 0.0053
      b_neurons = 0.0003

      memory_size = exp(baseline + b_nrow * nrow + b_n_layers * n_layers + b_neurons * neurons)
      memory_size = memory_size * 1.3 # scale by 30% to overestimate in most cases
      lg$info("Mlp memory size: %s MB", round(memory_size))
      ceiling(memory_size)
    }
  ),

  private = list(
    # nolint start: indentation_linter, line_length_linter
    .search_space = ps(
      mlp.n_layers = p_int(1L, 16L),
      mlp.neurons = p_int(1L, 1024L),
      mlp.p = p_dbl(0, 0.5),
      mlp.opt.lr = p_dbl(1e-5, 1e-2, logscale = TRUE),
      mlp.opt.weight_decay = p_dbl(1e-6, 1e-3, logscale = TRUE),
      mlp.epochs = p_int(1L, 100L, tags = "internal_tuning", aggr = function(x) as.integer(ceiling(mean(unlist(x)))))
    ),
    # nolint end

    .default_values = list(
      mlp.n_layers = 4L,
      mlp.neurons = 128L,
      mlp.p = 0.2,
      mlp.opt.lr = log(1e-5),
      mlp.opt.weight_decay = log(1e-6)
    )
  )
)

mlr_auto$add("mlp", function() AutoMlp$new())
