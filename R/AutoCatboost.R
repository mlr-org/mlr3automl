#' @title Catboost Auto
#'
#' @include mlr_auto.R
#'
#' @description
#' Catboost auto.
#'
#' @template param_id
#' @template param_task
#' @template param_measure
#' @template param_n_threads
#' @template param_timeout
#' @template param_devices
#'
#' @export
AutoCatboost = R6Class("AutoCatboost",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "catboost") {
      super$initialize(
        id = id,
        properties = c("internal_tuning", "large_data_sets"),
        task_types = c("classif", "regr"),
        packages = c("mlr3", "mlr3extralearners", "catboost"),
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

      # catboost only supports gpu via cuda
      task_type =  if ("cuda" %in% devices) "GPU" else "CPU"

      learner = lrn(sprintf("%s.catboost", task$task_type),
        id = "catboost",
        iterations = self$search_space(task)$upper["catboost.iterations"] %??% 1000L,
        early_stopping_rounds = self$early_stopping_rounds(task),
        use_best_model = TRUE,
        eval_metric = self$internal_measure(measure, task),
        task_type = task_type)
      set_threads(learner, n_threads)

      po("removeconstants", id = "catboost_removeconstants") %>>%
        po("colapply", id = "catboost_colapply", applicator = as.numeric, affect_columns = selector_type("integer")) %>>%
        po("removeconstants", id = "catboost_post_removeconstants") %>>%
        learner
    },

    #' @description
    #' Estimate the memory for the auto.
    estimate_memory = function(task) {
      upper = self$search_space(task)$upper

      # histogram size
      border_count = 254
      depth = upper["catboost.depth"]
      histogram_size = 20 * task$ncol * border_count * 2^depth

      # data size
      n_classes = if (inherits(task, "TaskClassif")) length(task$class_names) else 1
      data_set_size = task$nrow * task$ncol * 8
      data_size = data_set_size * 5 + data_set_size / 4 * n_classes

      memory_size = (histogram_size + data_size) / 1e6
      lg$info("Catboost memory size: %s MB", round(memory_size))
      ceiling(memory_size)
    },

    #' @description
    #' Get the internal measure for the auto.
    internal_measure = function(measure, task) {
      if (task$task_type == "regr") {
        switch(measure$id,
          "regr.rmse" = "RMSE",
          "regr.mae" = "MAE",
          "regr.mape" = "MAPE",
          "regr.smape" = "SMAPE",
          "regr.medae" = "MedianAbsoluteError",
          "regr.rsq" = "R2",  # regr.rsq has id `rsq`
          "rmse" # default
        )
      } else if ("twoclass" %in% task$properties) {
        switch(measure$id,
          "classif.ce" = "Accuracy",
          "classif.acc" = "Accuracy",
          "classif.bacc" = "BalancedAccuracy",
          "classif.auc" = "AUC",
          "classif.prauc" = "PRAUC",
          "classif.bbrier" = "BrierScore",
          "classif.logloss" = "Logloss",
          "classif.precision" = "Precision",
          "classif.recall" = "Recall",
          "classif.mcc" = "MCC",
          "error" # default
        )
      } else if ("multiclass" %in% task$properties) {
        switch(measure$id,
          "classif.ce" = "Accuracy",
          "classif.acc" = "Accuracy",
          "classif.mauc_mu" = "AUC",
          "classif.logloss" = "MultiClass",
          "classif.mcc" = "MCC",
          "merror" # default
        )
      }
    }
  ),

  private = list(
    .search_space =  ps(
      catboost.depth          = p_int(1, 12),
      catboost.learning_rate  = p_dbl(1e-3, 1, logscale = TRUE),
      catboost.l2_leaf_reg    = p_dbl(1e-3, 1e3),
      catboost.iterations     = p_int(1L, 1000L, tags = "internal_tuning", aggr = function(x) as.integer(ceiling(mean(unlist(x)))))
    ),

    .default_values = list(
      catboost.depth = 6L,
      catboost.learning_rate = log(0.03),
      catboost.l2_leaf_reg = 3
    )
  )
)

mlr_auto$add("catboost", function() AutoCatboost$new())


