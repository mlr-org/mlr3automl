#' @title Auto Learner
#'
#' @description
#' Abstract base class for Auto like learner.
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#' @param task_type (`character(1)`)\cr
#'   Type of task, e.g. `"regr"` or `"classif"`.
#'   Must be an element of [mlr_reflections$task_types$type][mlr_reflections].
#' @param learner_ids (`character()`)\cr
#'  List of learner ids.
#' @param graph ([mlr3pipelines::Graph]).
#'  Graph.
#' @param tuning_space (list of [paradox::TuneToken])\cr
#'  List of [paradox::TuneToken]s.
#' @param resampling ([mlr3::Resampling]).
#' @param measure ([mlr3::Measure]).
#' @param terminator ([bbotk::Terminator]).
#' @param callbacks (list of [mlr3tuning::CallbackTuning]).
#' @param learner_fallback ([mlr3::Learner]).
#' @param learner_timeout (`integer(1)`).
#'
#' @export
LearnerAutoBranch = R6Class("LearnerAutoBranch",
  inherit = Learner,
  public = list(

    #' @field learner_ids (`character()`).
    learner_ids = NULL,

    #' @field graph ([mlr3pipelines::Graph]).
    graph = NULL,

    #' @field resampling ([mlr3::Resampling]).
    resampling = NULL,

    #' @field measure ([mlr3::Measure]).
    measure = NULL,

    #' @field terminator ([bbotk::Terminator]).
    terminator = NULL,

    #' @field tuning_space (list of [TuneToken]).
    tuning_space = NULL,

    #' @field callbacks (list of [mlr3tuning::CallbackTuning]).
    callbacks = NULL,

    #' @field learner_fallback ([mlr3::Learner]).
    learner_fallback = NULL,

    #' @field learner_timeout (`numeric(1)`).
    learner_timeout = NULL,

    #' @field learner_memory_limit (`numeric(1)`).
    learner_memory_limit = NULL,

    instance = NULL,

    #' @field lhs_size (`integer(1)`).
    lhs_size = NULL,

    #' @field xgboost_eval_metric (`character(1)`).
    xgboost_eval_metric = NULL,

    #' @field catboost_eval_metric (`character(1)`).
    catboost_eval_metric = NULL,

    #' @field large_data_size (`numeric(1)`).
    large_data_size = NULL,

    #' @field large_data_nthread (`integer(1)`).
    large_data_nthread = NULL,

    #' @field small_data_size (`integer(1)`).
    small_data_size = NULL,

    #' @field small_data_resampling ([mlr3::Resampling]).
    small_data_resampling = NULL,

    max_cardinality = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id,
      task_type,
      learner_ids,
      graph,
      tuning_space,
      resampling,
      measure,
      terminator,
      callbacks = list(),
      learner_fallback = NULL,
      learner_timeout = Inf,
      learner_memory_limit = Inf,
      xgboost_eval_metric = NULL,
      catboost_eval_metric = NULL,
      lhs_size = 4L,
      large_data_size = 1e6,
      large_data_nthread = 1L,
      small_data_size = 5000L,
      small_data_resampling = rsmp("cv", folds = 5),
      max_cardinality = 100L
      ) {
      assert_choice(task_type, mlr_reflections$task_types$type)
      self$learner_ids = assert_character(learner_ids)
      self$graph = assert_graph(graph)
      self$tuning_space = assert_list(tuning_space)
      self$resampling = assert_resampling(resampling)
      self$measure = assert_measure(measure)
      self$terminator = assert_terminator(terminator)
      self$callbacks = assert_list(as_callbacks(callbacks), types = "CallbackTuning")
      self$learner_fallback = assert_learner(learner_fallback)
      self$learner_timeout = assert_numeric(learner_timeout)
      self$learner_memory_limit = assert_numeric(learner_memory_limit)
      self$lhs_size = assert_count(lhs_size)
      self$xgboost_eval_metric = assert_character(xgboost_eval_metric, null.ok = TRUE)
      self$catboost_eval_metric = assert_character(catboost_eval_metric, null.ok = TRUE)
      self$large_data_size = assert_numeric(large_data_size)
      self$large_data_nthread = assert_count(large_data_nthread)
      self$small_data_size = assert_count(small_data_size)
      self$small_data_resampling = assert_resampling(small_data_resampling)
      self$max_cardinality = assert_count(max_cardinality)

      # packages
      packages = unique(c("mlr3tuning", "mlr3learners", "mlr3pipelines", "mlr3mbo", "mlr3automl", graph$packages))

      super$initialize(
        id = id,
        task_type = task_type,
        packages = packages,
        feature_types = mlr_reflections$task_feature_types,
        predict_types = names(mlr_reflections$learner_predict_types[[task_type]]),
        properties = mlr_reflections$learner_properties[[task_type]],
      )
    }
  ),

  private = list(

    .train = function(task) {

      lg$debug("Training '%s' on task '%s'", self$id, task$id)

      # initialize mbo tuner
      tuner = tnr("adbo")

      # reduce number of workers on large data sets
      if (task$nrow * task$ncol > self$large_data_size) {
        self$learner_ids = c("lda", "ranger", "xgboost", "catboost")
        self$graph$param_set$set_values(xgboost.nthread = self$large_data_nthread)
        self$graph$param_set$set_values(ranger.num.threads = self$large_data_nthread)
        self$graph$param_set$set_values(catboost.thread_count = self$large_data_nthread)
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
      splits = partition(task, ratio = 0.9, stratify = TRUE)
      holdout_task = task$clone()
      holdout_task$filter(splits$test)

      # xgboost
      preproc = po("removeconstants", id = "pre_removeconstants") %>>%
        po("imputeoor", id = "xgboost_imputeoor") %>>%
        po("fixfactors", id = "xgboost_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "xgboost_imputesample") %>>%
        po("collapsefactors", target_level_count = self$max_cardinality, id = "xgboost_collapse") %>>%
        po("encode", method = "one-hot", id = "xgboost_encode") %>>%
        po("removeconstants", id = "xgboost_post_removeconstants")
      preproc$train(task)
      task$set_row_roles(splits$test, "holdout")
      holdout_task_xgboost = preproc$predict(holdout_task)[[1]]

      # catboost
      holdout_task_catboost = holdout_task$clone()

      # initialize graph learner
      graph_learner = as_learner(self$graph)
      graph_learner$id = "graph_learner"
      graph_learner$predict_type = self$measure$predict_type
      graph_learner$fallback = self$learner_fallback
      graph_learner$encapsulate = c(train = "callr", predict = "callr")
      graph_learner$timeout = c(train = self$learner_timeout, predict = self$learner_timeout)
      graph_learner$param_set$values$xgboost.holdout_task = holdout_task_xgboost
      graph_learner$param_set$values$xgboost.callbacks = list(cb.timeout(self$learner_timeout * 0.8))
      graph_learner$param_set$values$xgboost.eval_metric = self$xgboost_eval_metric
      graph_learner$memory_limit = c(train = self$learner_memory_limit, predict = self$learner_memory_limit)
      graph_learner$param_set$values$catboost.holdout_task = holdout_task_catboost
      graph_learner$param_set$values$catboost.eval_metric = self$catboost_eval_metric

      # initialize search space
      tuning_space = unlist(unname(self$tuning_space[self$learner_ids[self$learner_ids %in% names(self$tuning_space)]]), recursive = FALSE)
      graph_scratch = graph_learner$clone(deep = TRUE)
      graph_scratch$param_set$set_values(.values = tuning_space)
      graph_scratch$param_set$set_values(branch.selection = to_tune(self$learner_ids))
      search_space = graph_scratch$param_set$search_space()
      walk(self$learner_ids, function(learner_id) {
        param_ids = search_space$ids()
        param_ids = grep(paste0("^", learner_id), param_ids, value = TRUE)
        walk(param_ids, function(param_id) {
          search_space$add_dep(
            id = param_id,
            on = "branch.selection",
            cond = CondEqual$new(learner_id)
          )
        })
      })

      # initial design
      lhs_xdt = generate_lhs_design(self$lhs_size, self$task_type, self$learner_ids[self$learner_ids != "lda"], self$tuning_space)
      default_xdt = generate_default_design(self$task_type, self$learner_ids, task, self$tuning_space)
      initial_xdt = rbindlist(list(lhs_xdt, default_xdt), use.names = TRUE, fill = TRUE)
      setorderv(initial_xdt, "branch.selection")
      tuner$param_set$set_values(initial_design = initial_xdt)

      # initialize auto tuner
      self$instance = TuningInstanceRushSingleCrit$new(
        task = task,
        learner = graph_learner,
        resampling = resampling,
        measure = self$measure,
        terminator = self$terminator,
        search_space = search_space,
        callbacks = c(self$callbacks, clbk("mlr3automl.branch_nrounds")),
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
    },

    .predict = function(task) {
      lg$debug("Predicting with '%s' on task '%s'", self$id, task$id)
      self$model$graph_learner$predict(task)
    }
  )
)

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
LearnerClassifAutoBranch = R6Class("LearnerClassifAutoBranch",
  inherit = LearnerAutoBranch,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "classif.automl",
      resampling = rsmp("holdout"),
      measure = msr("classif.ce"),
      terminator = trm("evals", n_evals = 100L),
      callbacks = list(),
      learner_timeout = Inf,
      learner_memory_limit = Inf,
      nthread = 1L,
      lhs_size = 4L,
      xgboost_eval_metric = NULL,
      catboost_eval_metric = NULL,
      large_data_size = 1e6,
      large_data_nthread = 1L,
      small_data_size = 5000L,
      small_data_resampling = rsmp("cv", folds = 5),
      max_cardinality = 100L
      ) {
      assert_count(nthread)
      assert_count(max_cardinality)
      learner_ids = c("glmnet", "kknn", "lda", "nnet", "ranger", "svm", "xgboost", "catboost")

      # glmnet
      branch_glmnet = po("imputehist", id = "glmnet_imputehist") %>>%
        po("imputeoor", id = "glmnet_imputeoor") %>>%
        po("fixfactors", id = "glmnet_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "glmnet_imputesample") %>>%
        po("collapsefactors", target_level_count = max_cardinality, id = "glmnet_collapse") %>>%
        po("encode", method = "one-hot", id = "glmnet_encode") %>>%
        po("removeconstants", id = "glmnet_post_removeconstants") %>>%
        lrn("classif.glmnet", id = "glmnet")

      # kknn
      branch_kknn = po("imputehist", id = "kknn_imputehist") %>>%
        po("imputeoor", id = "kknn_imputeoor") %>>%
        po("fixfactors", id = "kknn_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "kknn_imputesample") %>>%
        po("collapsefactors", target_level_count = max_cardinality, id = "kknn_collapse") %>>%
        po("removeconstants", id = "kknn_post_removeconstants") %>>%
        lrn("classif.kknn", id = "kknn")

      # lda
      branch_lda = po("imputehist", id = "lda_imputehist") %>>%
        po("imputeoor", id = "lda_imputeoor") %>>%
        po("fixfactors", id = "lda_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "lda_imputesample") %>>%
        po("collapsefactors", target_level_count = max_cardinality, id = "lda_collapse") %>>%
        po("removeconstants", id = "lda_post_removeconstants") %>>%
        lrn("classif.lda", id = "lda")

      # nnet
      branch_nnet = po("imputehist", id = "nnet_imputehist") %>>%
        po("imputeoor", id = "nnet_imputeoor") %>>%
        po("fixfactors", id = "nnet_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "nnet_imputesample") %>>%
        po("collapsefactors", target_level_count = max_cardinality, id = "nnet_collapse") %>>%
        po("removeconstants", id = "nnet_post_removeconstants") %>>%
        lrn("classif.nnet", id = "nnet")

      # ranger
      branch_ranger = po("imputeoor", id = "ranger_imputeoor") %>>%
        po("fixfactors", id = "ranger_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "ranger_imputesample") %>>%
        po("collapsefactors", target_level_count = max_cardinality, id = "ranger_collapse") %>>%
        po("removeconstants", id = "ranger_post_removeconstants") %>>%
        lrn("classif.ranger", id = "ranger", num.threads = nthread)

      # svm
      branch_svm = po("imputehist", id = "svm_imputehist") %>>%
        po("imputeoor", id = "svm_imputeoor") %>>%
        po("fixfactors", id = "svm_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "svm_imputesample") %>>%
        po("collapsefactors", target_level_count = max_cardinality, id = "svm_collapse") %>>%
        po("encode", method = "one-hot", id = "smv_encode") %>>%
        po("removeconstants", id = "svm_post_removeconstants") %>>%
        lrn("classif.svm", id = "svm", type = "C-classification")

      # xgboost
      branch_xgboost = po("imputeoor", id = "xgboost_imputeoor") %>>%
        po("fixfactors", id = "xgboost_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "xgboost_imputesample") %>>%
        po("collapsefactors", target_level_count = max_cardinality, id = "xgboost_collapse") %>>%
        po("encode", method = "one-hot", id = "xgboost_encode") %>>%
        po("removeconstants", id = "xgboost_post_removeconstants") %>>%
        lrn("classif.xgboost", id = "xgboost", nrounds = 5000, early_stopping_rounds = 10, nthread = nthread)

      # catboost
      branch_catboost = lrn("classif.catboost", id = "catboost", thread_count = nthread, iterations = 1000, early_stopping_rounds = 10, use_best_model = TRUE)

      # branch graph
      graph = po("removeconstants", id = "pre_removeconstants") %>>%
        po("branch", options = learner_ids) %>>%
        gunion(list(branch_glmnet, branch_kknn, branch_lda, branch_nnet, branch_ranger, branch_svm, branch_xgboost, branch_catboost)) %>>% po("unbranch", options = learner_ids)

      learner_fallback = lrn("classif.featureless", predict_type = measure$predict_type)

      super$initialize(
        id = id,
        task_type = "classif",
        learner_ids = learner_ids,
        graph = graph,
        tuning_space = tuning_space,
        resampling = resampling,
        measure = measure,
        terminator = terminator,
        callbacks = callbacks,
        learner_fallback = learner_fallback,
        learner_timeout = learner_timeout,
        learner_memory_limit = learner_memory_limit,
        xgboost_eval_metric = xgboost_eval_metric,
        catboost_eval_metric = catboost_eval_metric,
        lhs_size = lhs_size,
        large_data_size = large_data_size,
        large_data_nthread = large_data_nthread,
        small_data_size = small_data_size,
        small_data_resampling = small_data_resampling,
        max_cardinality = max_cardinality)
    }
  )
)

tuning_space = list(
  glmnet = list(
    glmnet.s     = to_tune(1e-4, 1e4, logscale = TRUE),
    glmnet.alpha = to_tune(0, 1)
  ),

  kknn = list(
    kknn.k = to_tune(1, 50, logscale = TRUE),
    kknn.distance = to_tune(1, 5),
    kknn.kernel = to_tune(c("rectangular", "optimal", "epanechnikov", "biweight", "triweight", "cos",  "inv",  "gaussian", "rank"))
  ),

  nnet = list(
      nnet.maxit = to_tune(1e1, 1e3, logscale = TRUE),
      nnet.decay = to_tune(1e-4, 1e-1, logscale = TRUE),
      nnet.size  = to_tune(2, 50, logscale = TRUE)
  ),

  ranger = list(
    ranger.mtry.ratio      = to_tune(0, 1),
    ranger.replace         = to_tune(),
    ranger.sample.fraction = to_tune(1e-1, 1),
    ranger.num.trees       = to_tune(500, 2000)
  ),

  svm = list(
    svm.cost    = to_tune(1e-4, 1e4, logscale = TRUE),
    svm.kernel  = to_tune(c("polynomial", "radial", "sigmoid", "linear")),
    svm.degree  = to_tune(2, 5),
    svm.gamma   = to_tune(1e-4, 1e4, logscale = TRUE)
  ),

  xgboost = list(
    xgboost.eta               = to_tune(1e-4, 1, logscale = TRUE),
    xgboost.max_depth         = to_tune(1, 20),
    xgboost.colsample_bytree  = to_tune(1e-1, 1),
    xgboost.colsample_bylevel = to_tune(1e-1, 1),
    xgboost.lambda            = to_tune(1e-3, 1e3, logscale = TRUE),
    xgboost.alpha             = to_tune(1e-3, 1e3, logscale = TRUE),
    xgboost.subsample         = to_tune(1e-1, 1)
  ),

  catboost = list(
    catboost.depth          = to_tune(5, 8),
    catboost.learning_rate  = to_tune(5e-3, 0.2, logscale = TRUE),
    catboost.l2_leaf_reg    = to_tune(1, 5)
  )
)
