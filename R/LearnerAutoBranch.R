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
#' @param param_set ([ParamSet])\cr
#'  Parameter set.
#' @param graph ([mlr3pipelines::Graph]).
#'  Graph.
#' @param tuning_space (list of lists of [paradox::TuneToken])\cr
#'  List of tuning spaces.
#'
#' @export
LearnerAutoBranch = R6Class("LearnerAutoBranch",
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
    initialize = function(id, task_type, param_set, graph, tuning_space) {
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
      learner_ids = pv$learner_ids
      graph = self$graph

      lg$debug("Training '%s' on task '%s'", self$id, task$id)

      # initialize mbo tuner
      tuner = tnr("adbo")

      # remove learner based on memory limit
      memory_usage = map_dbl(learner_ids, function(learner_id) {
        graph$pipeops[[learner_id]]$learner$estimate_memory_usage(task)/1e6
      })
      learner_ids = learner_ids[memory_usage < pv$max_memory]

      # set number of threads
      walk(learner_ids, function(learner_id) {
        set_threads(graph$pipeops[[learner_id]]$learner, pv$max_nthread)
      })

      # reduce number of workers on large data sets
      if (task$nrow * task$ncol > pv$large_data_size) {
        learner_ids = intersect(learner_ids, pv$large_data_learner_ids)
        walk(learner_ids, function(learner_id) {
          set_threads(graph$pipeops[[learner_id]]$learner, pv$large_data_nthread)
        })
        n_workers = utils::getFromNamespace("rush_env", ns = "rush")$n_workers
        n = floor(n_workers / pv$large_data_nthread)
        lg$debug("Task larger than %i rows. Reducing number of workers to %i", pv$large_data_size, n)
        tuner$param_set$set_values(n_workers = n)
      }

      # small data resampling
      resampling = if (task$nrow < pv$small_data_size) {
        lg$debug("Task has less than %i rows, using small data resampling", pv$small_data_size)
        pv$small_data_resampling
      } else {
        pv$resampling
      }

      # cardinality
      if (any(map_int(task$col_info$levels, length) > pv$max_cardinality)) {
        lg$debug("Task has factors larger than %i levels", pv$max_cardinality)

        # collapse factors
        pipeop_ids = names(graph$pipeops)
        pipeop_ids[grep("collapse", pipeop_ids)]
        walk(pipeop_ids, function(pipeop_id) {
          graph$pipeops[[pipeop_id]]$param_set$values$target_level_count = pv$max_cardinality
        })
      }

      # initialize graph learner
      graph_learner = as_learner(graph)
      graph_learner$id = "graph_learner"
      graph_learner$predict_type = pv$measure$predict_type
      graph_learner$fallback = lrn("classif.featureless", predict_type = pv$measure$predict_type)
      graph_learner$encapsulate = c(train = "callr", predict = "callr")
      graph_learner$timeout = c(train = pv$learner_timeout, predict = pv$learner_timeout)

      # holdout task
      if (any(c("xgboost", "catboost") %in% learner_ids)) {
        lg$debug("Creating holdout task for xgboost and catboost")

        # holdout task
        splits = partition(task, ratio = 0.9, stratify = TRUE)
        holdout_task = task$clone()
        holdout_task$filter(splits$test)
        task$set_row_roles(splits$test, "holdout")

        if ("xgboost" %in% learner_ids) {
          # xgboost
          preproc = po("removeconstants", id = "pre_removeconstants") %>>%
            po("imputeoor", id = "xgboost_imputeoor") %>>%
            po("fixfactors", id = "xgboost_fixfactors") %>>%
            po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "xgboost_imputesample") %>>%
            po("encodeimpact", id = "xgboost_encode") %>>%
            po("removeconstants", id = "xgboost_post_removeconstants")
          preproc$train(task)
          graph_learner$param_set$values$xgboost.holdout_task = preproc$predict(holdout_task)[[1]]
          graph_learner$param_set$values$xgboost.callbacks = list(cb.timeout(pv$learner_timeout * 0.8))
          graph_learner$param_set$values$xgboost.eval_metric = pv$xgboost_eval_metric
        }

        if ("catboost" %in% learner_ids) {
          # catboost
          graph_learner$param_set$values$catboost.holdout_task = holdout_task$clone()
          graph_learner$param_set$values$catboost.eval_metric = pv$catboost_eval_metric
        }
      }

      # initialize search space
      tuning_space = unlist(unname(self$tuning_space[learner_ids[learner_ids %in% names(self$tuning_space)]]), recursive = FALSE)
      graph_scratch = graph_learner$clone(deep = TRUE)
      graph_scratch$param_set$set_values(.values = tuning_space)
      graph_scratch$param_set$set_values(branch.selection = to_tune(learner_ids))
      search_space = graph_scratch$param_set$search_space()
      walk(learner_ids, function(learner_id) {
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
      lhs_xdt = generate_lhs_design(pv$lhs_size, self$task_type, learner_ids[learner_ids %in% names(self$tuning_space)], self$tuning_space)
      default_xdt = generate_default_design(self$task_type, learner_ids, task, self$tuning_space)
      initial_xdt = rbindlist(list(lhs_xdt, default_xdt), use.names = TRUE, fill = TRUE)
      setorderv(initial_xdt, "branch.selection")
      tuner$param_set$set_values(initial_design = initial_xdt)

      # initialize auto tuner
      self$instance = TuningInstanceAsyncSingleCrit$new(
        task = task,
        learner = graph_learner,
        resampling = resampling,
        measure = pv$measure,
        terminator = pv$terminator,
        search_space = search_space,
        callbacks = c(pv$callbacks, clbk("mlr3automl.branch_nrounds")),
        store_benchmark_result = FALSE
      )

      # tune
      lg$debug("Learner '%s' starts tuning phase", self$id)
      tuner$optimize(self$instance)

      # fit final model
      lg$debug("Learner '%s' fits final model", self$id)
      if (any(c("xgboost", "catboost") %in% learner_ids)) task$set_row_roles(splits$test, "use")
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
#'
#' @export
LearnerClassifAutoBranch = R6Class("LearnerClassifAutoBranch",
  inherit = LearnerAutoBranch,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "classif.automl") {
      param_set = ps(
        # learner
        learner_ids = p_uty(tags = "required"),
        learner_timeout = p_int(lower = 1L, default = 900L, tags = "required"),
        xgboost_eval_metric = p_uty(),
        catboost_eval_metric = p_uty(),
        # system
        max_nthread = p_int(lower = 1L, default = 8L, tags = "required"),
        max_memory = p_int(lower = 1L, default = 320000L, tags = "required"),
        # large data
        large_data_size = p_int(lower = 1L, default = 1e6, tags = "required"),
        large_data_learner_ids = p_uty(tags = "required"),
        large_data_nthread = p_int(lower = 1L, default = 2L, tags = "required"),
        # small data
        small_data_size = p_int(lower = 1L, default = 5000L, tags = "required"),
        small_data_resampling = p_uty(tags = "required"),
        max_cardinality = p_int(lower = 1L, default = 100L, tags = "required"),
        # tuner
        resampling = p_uty(tags = "required"),
        terminator = p_uty(tags = "required"),
        measure = p_uty(tags = "required"),
        lhs_size = p_int(lower = 1L, default = 4L),
        callbacks = p_uty(),
        timeout = p_int(lower = 1L, default = 14400L, tags = "required"))

      learner_ids = c("glmnet", "kknn", "lda", "nnet", "ranger", "svm", "xgboost", "catboost", "extra_trees")
      param_set$set_values(
        learner_ids = learner_ids,
        learner_timeout = 900L,
        max_nthread = 8L,
        max_memory = 32000L,
        large_data_size = 1e6L,
        large_data_learner_ids = c("lda", "ranger", "xgboost", "catboost", "extra_trees"),
        large_data_nthread = 2L,
        small_data_size = 5000L,
        small_data_resampling = rsmp("cv", folds = 10L),
        max_cardinality = 100L,
        resampling = rsmp("cv", folds = 3L),
        terminator = trm("run_time", secs = 14400L),
        measure = msr("classif.ce"),
        lhs_size = 4L)

      # glmnet
      branch_glmnet = po("imputehist", id = "glmnet_imputehist") %>>%
        po("imputeoor", id = "glmnet_imputeoor") %>>%
        po("fixfactors", id = "glmnet_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "glmnet_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "glmnet_collapse") %>>%
        po("encode", method = "one-hot", id = "glmnet_encode") %>>%
        po("removeconstants", id = "glmnet_post_removeconstants") %>>%
        lrn("classif.glmnet", id = "glmnet")

      # kknn
      branch_kknn = po("imputehist", id = "kknn_imputehist") %>>%
        po("imputeoor", id = "kknn_imputeoor") %>>%
        po("fixfactors", id = "kknn_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "kknn_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "kknn_collapse") %>>%
        po("removeconstants", id = "kknn_post_removeconstants") %>>%
        lrn("classif.kknn", id = "kknn")

      # lda
      branch_lda = po("imputehist", id = "lda_imputehist") %>>%
        po("imputeoor", id = "lda_imputeoor") %>>%
        po("fixfactors", id = "lda_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "lda_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "lda_collapse") %>>%
        po("removeconstants", id = "lda_post_removeconstants") %>>%
        lrn("classif.lda", id = "lda")

      # nnet
      branch_nnet = po("imputehist", id = "nnet_imputehist") %>>%
        po("imputeoor", id = "nnet_imputeoor") %>>%
        po("fixfactors", id = "nnet_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "nnet_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "nnet_collapse") %>>%
        po("removeconstants", id = "nnet_post_removeconstants") %>>%
        lrn("classif.nnet", id = "nnet")

      # ranger
      branch_ranger = po("imputeoor", id = "ranger_imputeoor") %>>%
        po("fixfactors", id = "ranger_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "ranger_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "ranger_collapse") %>>%
        po("removeconstants", id = "ranger_post_removeconstants") %>>%
        lrn("classif.ranger", id = "ranger")

      # svm
      branch_svm = po("imputehist", id = "svm_imputehist") %>>%
        po("imputeoor", id = "svm_imputeoor") %>>%
        po("fixfactors", id = "svm_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "svm_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "svm_collapse") %>>%
        po("encode", method = "one-hot", id = "smv_encode") %>>%
        po("removeconstants", id = "svm_post_removeconstants") %>>%
        lrn("classif.svm", id = "svm", type = "C-classification")

      # xgboost
      branch_xgboost = po("imputeoor", id = "xgboost_imputeoor") %>>%
        po("fixfactors", id = "xgboost_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "xgboost_imputesample") %>>%
        po("encodeimpact", id = "xgboost_encode") %>>%
        po("removeconstants", id = "xgboost_post_removeconstants") %>>%
        lrn("classif.xgboost", id = "xgboost", nrounds = 5000, early_stopping_rounds = 10)

      # catboost
      branch_catboost = lrn("classif.catboost", id = "catboost", iterations = 500, early_stopping_rounds = 10, use_best_model = TRUE)

      # extra trees
      branch_extra_trees = po("imputeoor", id = "extra_trees_imputeoor") %>>%
        po("fixfactors", id = "extra_trees_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "extra_trees_imputesample") %>>%
        po("collapsefactors", target_level_count = 100, id = "extra_trees_collapse") %>>%
        po("removeconstants", id = "extra_trees_post_removeconstants") %>>%
        lrn("classif.ranger", id = "extra_trees", splitrule = "extratrees", num.trees = 100, replace = FALSE, sample.fraction = 1)

      # branch graph
      graph = po("removeconstants", id = "pre_removeconstants") %>>%
        po("branch", options = learner_ids) %>>%
        gunion(list(branch_glmnet, branch_kknn, branch_lda, branch_nnet, branch_ranger, branch_svm, branch_xgboost, branch_catboost, branch_extra_trees)) %>>% po("unbranch", options = learner_ids)

      super$initialize(
        id = id,
        task_type = "classif",
        param_set = param_set,
        graph = graph,
        tuning_space = tuning_space)
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

#' @include aaa.R
learners[["classif.automl_branch"]] = LearnerClassifAutoBranch
