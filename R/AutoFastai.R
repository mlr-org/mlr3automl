#' @title Fastai Auto
#'
#' @description
#' Fastai auto.
#'
#' @template param_id
#'
#' @include mlr_auto.R
#' @export
AutoFastai = R6Class("AutoFastai",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "fastai") {
      super$initialize(id = id)
      self$task_types = c("classif", "regr")
      self$properties = "internal_tuning"
    },

    #' @description
    #' Create the graph for the auto.
    #'
    #' @param task ([mlr3::Task]).
    #' @param measure ([mlr3::Measure]).
    #' @param n_threads (`numeric(1)`).
    #' @param timeout (`numeric(1)`).
    graph = function(task, measure, n_threads, timeout) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)

      learner = lrn(sprintf("%s.fastai", task$task_type),
        id = "fastai",
        patience = self$early_stopping_rounds(task),
        layers = self$default_values(task)$fastai.layers,
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
    #'
    #' @param task ([mlr3::Task]).
    estimate_memory = function(task) {
      memory_size = task$nrow * task$ncol * 10 / 1e6
      lg$info("Fastai memory size: %s MB", round(memory_size))
      memory_size
    },

    #' @description
    #' Get the internal measure for the auto.
    #'
    #' @param measure ([mlr3::Measure]).
    #' @param task ([mlr3::Task]).
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
    },

    #' @description
    #' Get the default values for the auto.
    #'
    #' @param task ([mlr3::Task]).
    default_values = function(task) {
      list(
        fastai.lr = log(1e-3),
        fastai.bs = 50,
        fastai.layers = "c(200, 200)"
      )
    }
  ),

  active = list(

    #' @field search_space (`ParamSet`).
    search_space = function() {
      ps(
        fastai.lr = p_dbl(1e-4, 1e-1, logscale = TRUE),
        fastai.bs = p_int(50, 500),
        fastai.n_epoch = p_int(1, 100, tags = "internal_tuning", aggr = function(x) as.integer(ceiling(mean(unlist(x))))),
        fastai.layers = p_fct(levels = list(c(200, 100), c(200, 100, 50), c(500, 200), c(500, 200, 100), c(1000, 500), c(1000, 500, 200)), default = "c(200, 100)")
      )
    }
  )
)

mlr_auto$add("fastai", function() AutoFastai$new())
