#' @title AutoML Single Learner
#'
#' @description
#' Abstract base class for an AutoML learner encapsulating a single [Learner][mlr3::Learner].
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#' @param task_type (`character(1)`)\cr
#'   Type of task, e.g. `"regr"` or `"classif"`.
#'   Must be an element of [mlr_reflections$task_types$type][mlr_reflections].
#' @param param_set ([ParamSet])\cr
#'  Parameter set.
#' @param graph ([mlr3pipelines::Graph]).
#'  Graph.
#' @param tuning_space (list of lists of [paradox::TuneToken])\cr
#'  List of tuning spaces.
#'
#' @export
LearnerAutoSingle = R6Class("LearnerAutoSingle",
  inherit = Learner,
  public = list(

    #' @field graph ([mlr3pipelines::Graph]).
    graph = NULL,

    #' @field tuning_space (`list()`).
    tuning_space = NULL,

    #' @field instance ([TuningInstanceRushSingleCrit]).
    instance = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, learner_id, task_type, param_set, graph, tuning_space) {
      self$graph = assert_graph(graph)
      self$tuning_space = assert_list(tuning_space)
      # packages
      packages = unique(c("mlr3tuning", "mlr3learners", "mlr3pipelines", "mlr3mbo", "mlr3automl", graph$packages))

      super$initialize(
        id = id,
        task_type = task_type,
        param_set = param_set,
        packages = packages,
        feature_types = mlr_reflections$task_feature_types,
        predict_types = names(mlr_reflections$learner_predict_types[[task_type]]),
        properties = mlr_reflections$learner_properties[[task_type]],
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$values
      learner_id = pv$learner_id
      graph = self$graph

      lg$debug("Training '%s' on task '%s'", self$id, task$id)

      # initialize mbo tuner
      tuner = tnr("adbo")

      # add memory limit check?

      # set number of threads
      lg$debug("Setting number of threads per learner to %i", pv$max_nthread)
      set_threads(graph$pipeops[[learner_id]]$learner, pv$max_nthread)

      # small data resampling
      resampling = if (task$nrow < pv$small_data_size) {
        lg$debug("Task has less than %i rows", pv$small_data_size)
        lg$debug("Using small data set resampling with %i iterations", pv$small_data_resampling$iters)
        pv$small_data_resampling
      } else {
        pv$resampling
      }

      # cardinality
      cardinality = map_int(task$col_info$levels, length)
      if (any(cardinality > pv$max_cardinality)) {
        lg$debug("Reducing number of factor levels to %i", pv$max_cardinality)

        # collapse factors
        pipeop_ids = names(graph$pipeops)
        pipeop_ids = pipeop_ids[grep("collapse", pipeop_ids)]
        walk(pipeop_ids, function(pipeop_id) {
          graph$pipeops[[pipeop_id]]$param_set$values$target_level_count = pv$max_cardinality
        })
      }

      if (learner_id == "extra_trees" && any(cardinality > pv$extra_trees_max_cardinality)) {
        lg$debug("Reducing number of factor levels to %i for extra trees", pv$extra_trees_max_cardinality)
        graph$pipeops$extra_trees_collapse$param_set$values$target_level_count = pv$extra_trees_max_cardinality
      }

      # initialize graph learner
      graph_learner = as_learner(graph)
      graph_learner$id = "graph_learner"
      graph_learner$predict_type = pv$measure$predict_type
      graph_learner$fallback = lrn("classif.featureless", predict_type = pv$measure$predict_type)
      graph_learner$encapsulate = c(train = "callr", predict = "callr")
      graph_learner$timeout = c(train = pv$learner_timeout, predict = pv$learner_timeout)
      if (learner_id %in% c("xgboost", "catboost", "lightgbm")) {
        set_validate(graph_learner, "test", ids = learner_id)
      }
      
      # set early stopping
      if (learner_id == "xgboost") {
        graph_learner$param_set$values$xgboost.callbacks = list(cb_timeout_xgboost(pv$learner_timeout * 0.8))
        graph_learner$param_set$values$xgboost.eval_metric = pv$xgboost_eval_metric
      }
      if (learner_id == "catboost") {
        graph_learner$param_set$values$catboost.eval_metric = pv$catboost_eval_metric
      }
      if (learner_id == "lightgbm") {
        graph_learner$param_set$values$lightgbm.callbacks = list(cb_timeout_lightgbm(pv$learner_timeout * 0.8))
        graph_learner$param_set$values$lightgbm.eval = pv$lightgbm_eval_metric
      }

      # initialize search space
      tuning_space = unlist(unname(self$tuning_space), recursive = FALSE)
      graph_scratch = graph_learner$clone(deep = TRUE)
      graph_scratch$param_set$set_values(.values = tuning_space)
      search_space = graph_scratch$param_set$search_space()

      # initial design
      lhs_xdt = generate_lhs_design(pv$lhs_size, self$task_type, learner_id, self$tuning_space)
      default_xdt = generate_default_design(self$task_type, learner_id, task, self$tuning_space)
      initial_xdt = rbindlist(list(lhs_xdt, default_xdt), use.names = TRUE, fill = TRUE)
      tuner$param_set$set_values(initial_design = initial_xdt)

      # initialize auto tuner
      self$instance = ti_async(
        task = task,
        learner = graph_learner,
        resampling = resampling,
        measure = pv$measure,
        terminator = pv$terminator,
        search_space = search_space,
        # callbacks might need adaptation
        callbacks = pv$callbacks,
        store_benchmark_result = pv$store_benchmark_result
      )

      # tune
      lg$debug("Learner '%s' starts tuning phase", self$id)
      tuner$optimize(self$instance)

      # fit final model
      lg$debug("Learner '%s' fits final model", self$id)
      if (learner_id %in% c("xgboost", "catboost", "lightgbm")) {
        set_validate(graph_learner, NULL, ids = learner_id)
      }
      graph_learner$param_set$set_values(.values = self$instance$result_learner_param_vals)
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
