
#' @title Classification Auto Learner
#'
#' @description
#' Classification Auto learner.
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#' @param resampling ([mlr3::Resampling]).
#' @param measure ([mlr3::Measure]).
#' @param terminator ([bbotk::Terminator]).
#' @param callbacks (list of [mlr3tuning::CallbackTuning]).
#' @param learner_timeout (`integer(1)`).
#' @param nthread (`integer(1)`).
#'
#' @export
LearnerClassifAutoXgboost = R6Class("LearnerClassifAutoXgboost",
  inherit = LearnerAuto,
  public = list(

    #' @field xgboost_eval_metric (`character(1)`).
    xgboost_eval_metric = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "classif.automl_xgboost",
      resampling = rsmp("holdout"),
      measure = msr("classif.ce"),
      terminator = trm("evals", n_evals = 100L),
      callbacks = list(),
      learner_timeout = Inf,
      learner_memory_limit = Inf,
      nthread = 1L,
      lhs_size = 4L,
      xgboost_eval_metric = NULL,
      large_data_size = 1e6,
      large_data_nthread = 1L,
      small_data_size = 5000L,
      small_data_resampling = rsmp("cv", folds = 5),
      max_cardinality = 100L
      ) {
      assert_count(nthread)
      assert_count(max_cardinality)
      learner_id = "xgboost"
      self$xgboost_eval_metric = assert_character(xgboost_eval_metric, null.ok = TRUE)

      # xgboost
      graph =
        po("removeconstants", id = "xgboost_pre_removeconstants") %>>%
        po("imputeoor", id = "xgboost_imputeoor") %>>%
        po("fixfactors", id = "xgboost_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "xgboost_imputesample") %>>%
        #po("collapsefactors", target_level_count = max_cardinality, id = "xgboost_collapse") %>>%
        po("encodeimpact") %>>%
        po("removeconstants", id = "xgboost_post_removeconstants") %>>%
        lrn("classif.xgboost", id = "xgboost", nrounds = 5000, early_stopping_rounds = 10, nthread = nthread)

      learner_fallback = lrn("classif.featureless", predict_type = measure$predict_type)

      super$initialize(
        id = id,
        task_type = "classif",
        learner_id = "xgboost",
        graph = graph,
        tuning_space = tuning_space,
        resampling = resampling,
        measure = measure,
        terminator = terminator,
        callbacks = callbacks,
        learner_fallback = learner_fallback,
        learner_timeout = learner_timeout,
        learner_memory_limit = learner_memory_limit,
        lhs_size = lhs_size,
        large_data_size = large_data_size,
        large_data_nthread = large_data_nthread,
        small_data_size = small_data_size,
        small_data_resampling = small_data_resampling,
        max_cardinality = max_cardinality)
    }
  ),

  private = list(

    .train = function(task) {

      lg$debug("Training '%s' on task '%s'", self$id, task$id)

      # initialize mbo tuner
      tuner = tnr("adbo")

      # reduce number of workers on large data sets
      if (task$nrow * task$ncol > self$large_data_size) {
        self$graph$param_set$set_values(xgboost.nthread = self$large_data_nthread)
        n_workers = utils::getFromNamespace("rush_env", ns = "rush")$n_workers
        n = floor(n_workers / self$large_data_nthread)
        lg$debug("Task larger than %i rows. Reducing number of workers to %i", self$large_data_size, n)
        tuner$param_set$set_values(n_workers = n)
      }

      # small data resampling
      resampling = if (task$nrow < self$small_data_size) {
        lg$debug("Task has less than %i rows, using small data resampling", self$small_data_size)
        self$small_data_resampling
      } else {
        self$resampling
      }

      # cardinality
      if (any(map_int(task$col_info$levels, length) > self$max_cardinality)) {
        lg$debug("Task has factors larger than %i levels", self$max_cardinality)
      }

      # holdout task
      preproc = po("removeconstants", id = "xgboost_pre_removeconstants") %>>%
        po("imputeoor", id = "xgboost_imputeoor") %>>%
        po("fixfactors", id = "xgboost_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "xgboost_imputesample") %>>%
        #po("collapsefactors", target_level_count = self$max_cardinality, id = "xgboost_collapse") %>>%
        po("encodeimpact") %>>%
        po("removeconstants", id = "xgboost_post_removeconstants")
      splits = partition(task, ratio = 0.9, stratify = TRUE)
      holdout_task = task$clone()
      holdout_task$filter(splits$test)
      preproc$train(task)
      holdout_task = preproc$predict(holdout_task)[[1]]
      task$set_row_roles(splits$test, "holdout")

      # initialize graph learner
      graph_learner = as_learner(self$graph)
      graph_learner$id = "graph_learner"
      graph_learner$predict_type = self$measure$predict_type
      graph_learner$fallback = self$learner_fallback
      graph_learner$encapsulate = c(train = "callr", predict = "callr")
      graph_learner$timeout = c(train = self$learner_timeout, predict = self$learner_timeout)
      graph_learner$param_set$values$xgboost.holdout_task = holdout_task
      graph_learner$param_set$values$xgboost.callbacks = list(cb.timeout(self$learner_timeout * 0.8))
      graph_learner$param_set$values$xgboost.eval_metric = self$xgboost_eval_metric
      graph_learner$memory_limit = c(train = self$learner_memory_limit, predict = self$learner_memory_limit)
      graph_learner$param_set$set_values(.values = unlist(unname(tuning_space), recursive = FALSE))

      # initial design
      lhs_xdt = generate_lhs_design(self$lhs_size, self$task_type, self$learner_id, self$tuning_space, branch = FALSE)
      default_xdt = generate_default_design(self$task_type, self$learner_id, task, self$tuning_space, branch = FALSE)
      initial_xdt = rbindlist(list(lhs_xdt, default_xdt), use.names = TRUE, fill = TRUE)
      tuner$param_set$set_values(initial_design = initial_xdt)

      # initialize auto tuner
      self$instance = TuningInstanceRushSingleCrit$new(
        task = task,
        learner = graph_learner,
        resampling = resampling,
        measure = self$measure,
        terminator = self$terminator,
        callbacks = c(self$callbacks, clbk("mlr3automl.nrounds")),
        store_benchmark_result = FALSE
      )

      # tune
      lg$debug("Learner '%s' starts tuning phase", self$id)
      tuner$optimize(self$instance)

      # fit final model
      lg$debug("Learner '%s' fits final model", self$id)
      task$set_row_roles(splits$test, "use")
      graph_learner$param_set$set_values(.values = self$instance$result_learner_param_vals)
      graph_learner$timeout = c(train = Inf, predict = Inf)
      graph_learner$train(task)

      list(graph_learner = graph_learner, instance = self$instance)
    }
  )
)

tuning_space = list(
  xgboost = list(
    xgboost.eta               = to_tune(1e-4, 1, logscale = TRUE),
    xgboost.max_depth         = to_tune(1, 20),
    xgboost.colsample_bytree  = to_tune(1e-1, 1),
    xgboost.colsample_bylevel = to_tune(1e-1, 1),
    xgboost.lambda            = to_tune(1e-3, 1e3, logscale = TRUE),
    xgboost.alpha             = to_tune(1e-3, 1e3, logscale = TRUE),
    xgboost.subsample         = to_tune(1e-1, 1)
  )
)
