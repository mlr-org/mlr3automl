#' @title Lightgbm Auto
#'
#' @include mlr_auto.R
#'
#' @description
#' Lightgbm auto.
#'
#' @template param_id
#' @template param_task
#' @template param_measure
#' @template param_n_threads
#' @template param_timeout
#' @template param_devices
#'
#' @export
AutoLightgbm = R6Class("AutoLightgbm",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "lightgbm") {
      super$initialize(id = id,
        properties = c("internal_tuning", "large_data_sets"),
        task_types = c("classif", "regr"),
        packages = c("mlr3", "mlr3extralearners", "lightgbm"),
        devices = c("cpu", "cuda")
      )
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

      device_type = if ("cuda" %in% devices) "gpu" else "cpu"

      learner = lrn(sprintf("%s.lightgbm", task$task_type),
        id = "lightgbm",
        early_stopping_rounds = self$early_stopping_rounds(task),
        callbacks = list(cb_timeout_lightgbm(timeout * 0.8)),
        eval = self$internal_measure(measure, task),
        device_type = device_type)
      set_threads(learner, n_threads)

      learner
    },

    #' @description
    #' Estimate the memory for the auto.
    estimate_memory = function(task) {
        upper = self$search_space(task)$upper

        # histogram size
        max_bin =  255
        num_leaves = upper["lightgbm.num_leaves"]
        histogram_size = 20 * task$ncol * num_leaves * max_bin

        # data size
        data_set_size = task$nrow * task$ncol * 8
        data_size = data_set_size * 5 + data_set_size / 4 * length(task$class_names)

        memory_size = (histogram_size + data_size) / 1e6
        lg$info("Lightgbm memory size: %s MB", round(memory_size))
        ceiling(memory_size)
    },

    #' @description
    #' Get the internal measure for the auto.
    internal_measure = function(measure, task) {
       if (task$task_type == "regr") {
        switch(measure$id,
          "regr.mse" = "mse",
          "regr.rmse" = "rmse",
          "regr.mae" = "mae",
          "regr.mape" = "mape",
          "rmse") # default
      } else if ("twoclass" %in% task$properties) {
        switch(measure$id,
          "classif.ce" = "binary_error",
          "classif.acc" = "binary_error",
          "classif.auc" = "auc",
          "classif.logloss" = "binary_logloss",
          "binary_error") # default
      } else if ("multiclass" %in% task$properties) {
        switch(measure$id,
          "classif.ce" = "multi_error",
          "classif.acc" = "multi_error",
          "classif.mauc_mu" = "auc_mu",
          "classif.logloss" = "multi_logloss",
          "multi_error") # default
      }
    }
  ),

  private = list(
    .search_space = ps(
        lightgbm.learning_rate    = p_dbl(1e-3, 1, logscale = TRUE),
        lightgbm.feature_fraction = p_dbl(0.1, 1),
        lightgbm.min_data_in_leaf = p_int(1L, 200L),
        lightgbm.num_leaves       = p_int(10L, 255L),
        lightgbm.num_iterations   = p_int(1L, 5000L, tags = "internal_tuning", aggr = function(x) as.integer(ceiling(mean(unlist(x)))))
    ),

    .default_values = list(
      lightgbm.learning_rate = log(0.1),
      lightgbm.feature_fraction = 1,
      lightgbm.min_data_in_leaf = 20L,
      lightgbm.num_leaves = 31L
    )
  )
)

mlr_auto$add("lightgbm", function() AutoLightgbm$new())


