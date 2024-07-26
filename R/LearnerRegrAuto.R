#' @title Regression Auto Learner
#'
#' @description
#' Regression auto learner.
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#'
#' @export
LearnerRegrAuto = R6Class("LearnerRegrAuto",
  inherit = Learner,
  public = list(

    #' @field graph ([mlr3pipelines::Graph]).
    graph = NULL,

    #' @field tuning_space (`list()`).
    tuning_space = NULL,

    #' @field instance ([mlr3tuning::TuningInstanceAsyncSingleCrit]).
    instance = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.auto") {
      param_set = ps(
        # learner
        learner_ids = p_uty(default = c("glmnet", "kknn", "nnet", "ranger", "svm", "xgboost", "catboost", "extra_trees", "lightgbm"),
          custom_check = function(x) {
            if (length(x) == 1 && x == "extra_trees") {
              return("Learner 'extra_trees' must be combined with other learners")
            }
            check_subset(x, c("glmnet", "kknn", "nnet", "ranger", "svm", "xgboost", "catboost", "extra_trees", "lightgbm"))
        }),
        learner_timeout = p_int(lower = 1L, default = 900L),
        xgboost_eval_metric = p_uty(),
        catboost_eval_metric = p_uty(),
        lightgbm_eval_metric = p_uty(),
        # system
        max_nthread = p_int(lower = 1L, default = 1L),
        max_memory = p_int(lower = 1L, default = 32000L),
        # large data
        large_data_size = p_int(lower = 1L, default = 1e6),
        large_data_learner_ids = p_uty(),
        large_data_nthread = p_int(lower = 1L, default = 4L),
        # small data
        small_data_size = p_int(lower = 1L, default = 5000L),
        small_data_resampling = p_uty(),
        max_cardinality = p_int(lower = 1L, default = 100L),
        extra_trees_max_cardinality = p_int(lower = 1L, default = 40L),
        # tuner
        resampling = p_uty(),
        terminator = p_uty(),
        measure = p_uty(),
        lhs_size = p_int(lower = 1L, default = 4L),
        callbacks = p_uty(),
        store_benchmark_result = p_lgl(default = FALSE))

      param_set$set_values(
        learner_ids = c("glmnet", "kknn", "nnet", "ranger", "svm", "xgboost", "catboost", "extra_trees", "lightgbm"),
        learner_timeout = 900L,
        max_nthread = 1L,
        max_memory = 32000L,
        large_data_size = 1e6L,
        large_data_learner_ids = c("ranger", "xgboost", "catboost", "extra_trees", "lightgbm"),
        large_data_nthread = 4L,
        small_data_size = 5000L,
        small_data_resampling = rsmp("cv", folds = 10L),
        max_cardinality = 100L,
        extra_trees_max_cardinality = 40L,
        resampling = rsmp("cv", folds = 3L),
        terminator = trm("run_time", secs = 14400L),
        measure = msr("regr.mse"),
        lhs_size = 4L,
        store_benchmark_result = FALSE)

      super$initialize(
        id = id,
        task_type = "regr",
        param_set = param_set,
        packages = c("mlr3tuning", "mlr3learners", "mlr3pipelines", "mlr3mbo", "mlr3automl", "xgboost", "catboost", "lightgbm", "ranger", "nnet", "kknn", "glmnet", "e1071"),
        feature_types = c("logical", "integer", "numeric", "character", "factor"),
        predict_types = "response",
        properties = c("missings", "weights")
      )
    }
  ),
  private = list(
    .train = function(task) {
      pv = self$param_set$values
      learner_ids = pv$learner_ids
      self$graph = build_graph(learner_ids, task_type = "regr")
      self$tuning_space = tuning_space[learner_ids]

      lg$debug("Training '%s' on task '%s'", self$id, task$id)

      # initialize mbo tuner
      tuner = tnr("adbo")

      # remove learner based on memory limit
      lg$debug("Starting to select from %i learners: %s", length(learner_ids), paste0(learner_ids, collapse = ","))

      if (!is.null(pv$max_memory)) {
        memory_usage = map_dbl(learner_ids, function(learner_id) {
          self$graph$pipeops[[learner_id]]$learner$estimate_memory_usage(task) / 1e6
        })
        learner_ids = learner_ids[memory_usage < pv$max_memory]
        lg$debug("Checking learners for memory limit of %i MB. Keeping %i learner(s): %s", pv$max_memory, length(learner_ids), paste0(learner_ids, collapse = ","))
      }

      # set number of threads
      if (!is.null(pv$max_nthread)) {
        lg$debug("Setting number of threads per learner to %i", pv$max_nthread)
        walk(learner_ids, function(learner_id) {
          set_threads(self$graph$pipeops[[learner_id]]$learner, pv$max_nthread)
        })
      }

      # reduce number of workers on large data sets
      if (!is.null(pv$large_data_size) && task$nrow * task$ncol > pv$large_data_size) {
        lg$debug("Task size larger than %i rows", pv$large_data_size)

        learner_ids = intersect(learner_ids, pv$large_data_learner_ids)
        self$tuning_space = tuning_space[learner_ids]
        lg$debug("Keeping %i learner(s): %s", length(learner_ids), paste0(learner_ids, collapse = ","))

        lg$debug("Increasing number of threads per learner to %i", pv$large_data_nthread)
        walk(learner_ids, function(learner_id) {
          set_threads(self$graph$pipeops[[learner_id]]$learner, pv$large_data_nthread)
        })
        n_workers = rush_config()$n_workers
        n = max(1, floor(n_workers / pv$large_data_nthread))
        tuner$param_set$set_values(n_workers = n)
        lg$debug("Reducing number of workers to %i", n)
      }

      # small data resampling
      resampling = if (!is.null(pv$small_data_size) && task$nrow < pv$small_data_size) {
        lg$debug("Task has less than %i rows", pv$small_data_size)
        lg$debug("Using small data set resampling with %i iterations", pv$small_data_resampling$iters)
        pv$small_data_resampling
      } else {
        pv$resampling
      }

      # cardinality
      cardinality = map_int(task$col_info$levels, length)
      if (!is.null(pv$max_cardinality) && any(cardinality > pv$max_cardinality)) {
        lg$debug("Reducing number of factor levels to %i", pv$max_cardinality)

        # collapse factors
        pipeop_ids = names(self$graph$pipeops)
        pipeop_ids = pipeop_ids[grep("collapse", pipeop_ids)]
        walk(pipeop_ids, function(pipeop_id) {
          self$graph$pipeops[[pipeop_id]]$param_set$values$target_level_count = pv$max_cardinality
        })
      }

      if ("extra_trees" %in% learner_ids && any(cardinality > pv$extra_trees_max_cardinality))  {
        lg$debug("Reducing number of factor levels to %i for extra trees", pv$extra_trees_max_cardinality)
        self$graph$pipeops$extra_trees_collapse$param_set$values$target_level_count = pv$extra_trees_max_cardinality
      }

      # initialize graph learner
      graph_learner = as_learner(self$graph)
      graph_learner$id = "graph_learner"
      graph_learner$predict_type = pv$measure$predict_type
      graph_learner$fallback = lrn("regr.featureless", predict_type = pv$measure$predict_type)
      graph_learner$encapsulate = c(train = "callr", predict = "callr")
      graph_learner$timeout = c(train = pv$learner_timeout, predict = pv$learner_timeout)

      learners_with_validation = intersect(learner_ids, c("xgboost", "catboost", "lightgbm"))
      if (length(learners_with_validation)) {
        set_validate(graph_learner, "test", ids = learners_with_validation)
      }

      # set early stopping
      if ("xgboost" %in% learner_ids) {
        graph_learner$param_set$values$xgboost.callbacks = list(cb_timeout_xgboost(pv$learner_timeout * 0.8))
        graph_learner$param_set$values$xgboost.eval_metric = pv$xgboost_eval_metric
      }
      if ("catboost" %in% learner_ids) {
        graph_learner$param_set$values$catboost.eval_metric = pv$catboost_eval_metric
      }
      if ("lightgbm" %in% learner_ids) {
        graph_learner$param_set$values$lightgbm.callbacks = list(cb_timeout_lightgbm(pv$learner_timeout * 0.8))
        graph_learner$param_set$values$lightgbm.eval = pv$lightgbm_eval_metric
      }

      # initialize search space
      tuning_space = unlist(unname(self$tuning_space), recursive = FALSE)
      graph_scratch = graph_learner$clone(deep = TRUE)
      graph_scratch$param_set$set_values(.values = tuning_space)
      graph_scratch$param_set$set_values(branch.selection = to_tune(learner_ids))
      search_space = graph_scratch$param_set$search_space()
      walk(learner_ids, function(learner_id) {
        param_ids = search_space$ids()
        param_ids = grep(paste0("^", learner_id), param_ids, value = TRUE)
        walk(param_ids, function(param_id) {
          # skip internal tuning parameter
          if (param_id %in% c("xgboost.nrounds", "catboost.iterations", "lightgbm.num_iterations")) return()
          search_space$add_dep(
            id = param_id,
            on = "branch.selection",
            cond = CondEqual$new(learner_id)
          )
        })
      })

      # initial design
      lhs_xdt = generate_lhs_design(pv$lhs_size, self$task_type, setdiff(learner_ids, "extra_trees"), self$tuning_space)
      default_xdt = generate_default_design(self$task_type, learner_ids, task, self$tuning_space)
      initial_xdt = rbindlist(list(lhs_xdt, default_xdt), use.names = TRUE, fill = TRUE)
      setorderv(initial_xdt, "branch.selection")
      tuner$param_set$set_values(initial_design = initial_xdt)

      # initialize auto tuner
      self$instance = ti_async(
        task = task,
        learner = graph_learner,
        resampling = resampling,
        measures = pv$measure,
        terminator = pv$terminator,
        search_space = search_space,
        callbacks = pv$callbacks,
        store_benchmark_result = pv$store_benchmark_result
      )

      # tune
      lg$debug("Learner '%s' starts tuning phase", self$id)
      tuner$optimize(self$instance)

      # fit final model
      lg$debug("Learner '%s' fits final model", self$id)
      if (length(learners_with_validation)) {
        set_validate(graph_learner, NULL, ids = intersect(learner_ids, c("xgboost", "catboost", "lightgbm")))
      }
      graph_learner$param_set$set_values(.values = self$instance$result_learner_param_vals, .insert = FALSE)
      graph_learner$timeout = c(train = Inf, predict = Inf)
      graph_learner$train(task)

      list(graph_learner = graph_learner, instance = self$instance)
    },

    .predict = function(task) {
      lg$debug("Predicting with '%s' on task '%s'", self$id, task$id)
      self$model$graph_learner$predict(task)
    }
  )
)

#' @include aaa.R
learners[["regr.auto"]] = LearnerRegrAuto
