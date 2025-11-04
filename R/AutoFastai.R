#' @title Fastai Auto
#'
#' @include mlr_auto.R
#'
#' @description
#' Fastai auto.
#'
#' @template param_id
#' @template param_task
#' @template param_measure
#' @template param_n_threads
#' @template param_timeout
#' @template param_memory_limit
#' @template param_large_data_set
#' @template param_devices
#'
#' @export
AutoFastai = R6Class("AutoFastai",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "fastai") {
      super$initialize(
        id = id,
        properties = "internal_tuning",
        task_types = "classif",
        packages = c("mlr3", "mlr3extralearners", "fastai"),
        devices = c("cpu", "cuda")
      )
    },

    #' @description
    #' Check if the auto is compatible with the task.
    check = function(task, memory_limit = Inf, large_data_set = FALSE, devices = "cpu") {
      ok = super$check(task, memory_limit, large_data_set, devices)
      if (!isTRUE(ok)) {
        return(FALSE)
      }
      ok = check_python_packages(c("fastai", "torch"))
      if (!isTRUE(ok)) {
        lg$info(ok)
        lg$info("Remove fastai from search space")
        return(FALSE)
      }
      TRUE
    },

    #' @description
    #' Create the graph for the auto.
    graph = function(task, measure, n_threads, timeout, devices) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)
      assert_subset(devices, self$devices)

      require_namespaces("mlr3extralearners")

      learner = lrn(sprintf("%s.fastai", task$task_type),
        id = "fastai",
        patience = self$early_stopping_rounds(task),
        layers = c(200, 100),
        eval_metric = self$internal_measure(measure, task)
      )
      set_threads(learner, n_threads)

      po("removeconstants", id = "fastai_removeconstants") %>>%
        po("imputeoor", id = "fastai_imputeoor") %>>%
        po("fixfactors", id = "fastai_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "fastai_imputesample") %>>%
        po("encodeimpact", id = "fastai_encode") %>>%
        po("removeconstants", id = "fastai_post_removeconstants") %>>%
        learner
    },

    #' @description
    #' Estimate the memory for the auto.
    estimate_memory = function(task) {
      memory_size = task$nrow * task$ncol * 8 * 10 / 1e6
      lg$info("Fastai memory size: %s MB", round(memory_size))
      ceiling(memory_size)
    },

    #' @description
    #' Get the internal measure for the auto.
    internal_measure = function(measure, task) {
      if ("twoclass" %in% task$properties) {
        switch(measure$id,
          "classif.ce" = "error_rate",
          "classif.acc" = "accuracy",
          "classif.bacc" = "BalancedAccuracy",
          "classif.brier" = "BrierScore",
          "classif.auc" = "RocAucBinary",
          "classif.mcc" = "MatthewsCorrCoef",
          "error_rate" # default
        )
      } else if ("multiclass" %in% task$properties) {
        switch(measure$id,
          "classif.ce" = "error_rate",
          "classif.acc" = "accuracy",
          "classif.mauc_aunp" = "RocAuc",
          "error_rate" # default
        )
      }
    }
  ),
  private = list(
    .default_values = list(
      fastai.lr = log(1e-3),
      fastai.bs = 50,
      fastai.layers = "c(200, 200)"
    ),

    .search_space = ps(
      fastai.lr = p_dbl(1e-4, 1e-1, logscale = TRUE),
      fastai.bs = p_int(50, 500),
      fastai.n_epoch = p_int(1, 100, tags = "internal_tuning", aggr = function(x) as.integer(ceiling(mean(unlist(x))))),
      fastai.layers = p_fct(levels = list(c(200, 100), c(200, 100, 50), c(500, 200), c(500, 200, 100), c(1000, 500), c(1000, 500, 200)), default = "c(200, 100)")
    )
  )
)

mlr_auto$add("fastai", function() AutoFastai$new())
