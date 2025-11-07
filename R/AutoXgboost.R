#' @title Xgboost Auto
#'
#' @include mlr_auto.R Auto.R
#'
#' @description
#' Xgboost auto.
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
AutoXgboost = R6Class("AutoXgboost",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "xgboost") {
      super$initialize(id = id,
        properties = c("internal_tuning", "large_data_sets"),
        task_types = c("classif", "regr"),
        packages = c("mlr3", "mlr3learners", "xgboost"),
        devices = c("cpu", "cuda")
      )
    },

    #' @description
    #' Create the graph for the auto.
    graph = function(task, measure, n_threads, timeout, devices, pv) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)
      assert_subset(devices, self$devices)

      require_namespaces("mlr3learners")

      learner = lrn(sprintf("%s.xgboost", task$task_type),
        id = "xgboost",
        early_stopping_rounds = self$early_stopping_rounds(task),
        callbacks = list(cb_timeout_xgboost(timeout * 0.9)),
        eval_metric = self$internal_measure(measure, task),
        nrounds = 5000L
      )
      set_threads(learner, n_threads)

      if ("cuda" %in% devices) {
        learner$set_values(device = "cuda")
      }

      graph = po("removeconstants", id = "xgboost_removeconstants") %>>%
        po("imputeoor", id = "xgboost_imputeoor") %>>%
        po("fixfactors", id = "xgboost_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "xgboost_imputesample") %>>%
        po("encodeimpact", id = "xgboost_encode") %>>%
        po("removeconstants", id = "xgboost_post_removeconstants") %>>%
        learner

      if (task$nrow * task$ncol > pv$large_data_size) {
        graph = po("subsample", frac = 0.25, stratify = inherits(task, "TaskClassif"), use_groups = FALSE, id = "xgboost_subsample") %>>% graph
      }

      graph
    },

    #' @description
    #' Estimate the memory for the auto.
    estimate_memory = function(task) {
      upper = self$search_space(task)$upper

      # histogram size
      max_depth = upper["xgboost.max_depth"]
      max_bin = 265
      if (max_depth < 6) max_depth = 6
      histogram_size = max_bin * task$ncol * 2^max_depth

      # data size
      data_size = task$nrow * task$ncol * 8

      memory_size = (histogram_size + data_size) / 1e6
      lg$info("Xgboost memory size: %s MB", round(memory_size))
      ceiling(memory_size)
    },

    #' @description
    #' Get the internal measure for the auto.
    internal_measure = function(measure, task) {
      if (task$task_type == "regr") {
        switch(measure$id,
          "regr.rmse" = "rmse",
          "regr.rmsle" = "rmsle",
          "regr.mae" = "mae",
          "regr.mape" = "mape",
          "rmse" # default
        )
      } else if ("twoclass" %in% task$properties) {
        switch(measure$id,
          "classif.ce" = "error",
          "classif.acc" = "error",
          "classif.auc" = "auc",
          "classif.prauc" = "aucpr",
          "classif.logloss" = "logloss",
          "error" # default
        )
      } else if ("multiclass" %in% task$properties) {
        switch(measure$id,
          "classif.ce" = "merror",
          "classif.acc" = "merror",
          "classif.mauc_aunp" = "auc",
          "classif.logloss" = "mlogloss",
          "merror" # default
        )
      }
    },

    #' @description
    #' Modify the graph for the final model.
    final_graph = function(graph, task, pv) {
      if (task$nrow * task$ncol > pv$large_data_size) {
        graph$param_set$set_values(xgboost_subsample.frac = 1, xgboost.callbacks = NULL)
      }
    }
  ),

  private = list(
    .search_space = ps(
        xgboost.eta               = p_dbl(1e-4, 1, logscale = TRUE),
        xgboost.max_depth         = p_int(1, 12),
        xgboost.colsample_bytree  = p_dbl(1e-1, 1),
        xgboost.colsample_bylevel = p_dbl(1e-1, 1),
        xgboost.lambda            = p_dbl(1e-3, 1e3, logscale = TRUE),
        xgboost.alpha             = p_dbl(1e-3, 1e3, logscale = TRUE),
        xgboost.subsample         = p_dbl(1e-1, 1),
        xgboost.nrounds           = p_int(1, 5000, tags = "internal_tuning", aggr = function(x) as.integer(ceiling(mean(unlist(x)))))
    ),

    .default_values = list(
      xgboost.eta = log(0.3),
      xgboost.max_depth = 6L,
      xgboost.colsample_bytree = 1L,
      xgboost.colsample_bylevel = 1L,
      xgboost.lambda = log(1L),
      xgboost.alpha = log(0L),
      xgboost.subsample = 1L
    )
  )
)

mlr_auto$add("xgboost", function() AutoXgboost$new())
