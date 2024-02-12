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
LearnerAuto = R6Class("LearnerAuto",
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

    #' @field learner_timeout (`integer(1)`).
    learner_timeout = NULL,

    instance = NULL,

    lhs_size = NULL,

    #' @field xgboost_eval_metric (`character(1)`).
    xgboost_eval_metric = NULL,

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
      lhs_size = 4L,
      xgboost_eval_metric = NULL
      ) {
      assert_choice(task_type, mlr_reflections$task_types$type)
      self$learner_ids = assert_character(learner_ids)
      self$graph = assert_graph(graph)
      self$tuning_space = assert_list(tuning_space, types = "TuneToken")
      self$resampling = assert_resampling(resampling)
      self$measure = assert_measure(measure)
      self$terminator = assert_terminator(terminator)
      self$callbacks = assert_list(as_callbacks(callbacks), types = "CallbackTuning")
      self$learner_fallback = assert_learner(learner_fallback)
      self$learner_timeout = assert_numeric(learner_timeout)
      self$lhs_size = assert_count(lhs_size)
      self$xgboost_eval_metric = assert_character(xgboost_eval_metric, null.ok = TRUE)

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

      # holdout task
      preproc = po("removeconstants", id = "pre_removeconstants") %>>%
        po("imputeoor", id = "xgboost_imputeoor") %>>%
        po("encode", method = "one-hot", id = "xgboost_encode") %>>%
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

      # initialize search space
      graph_scratch = graph_learner$clone(deep = TRUE)
      graph_scratch$param_set$set_values(.values = self$tuning_space)
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
      lhs_xdt = generate_lhs_design(self$lhs_size, self$task_type, self$learner_ids, self$tuning_space)
      default_xdt = generate_default_design(self$task_type, self$learner_ids, task, self$tuning_space)
      initial_xdt = rbindlist(list(lhs_xdt, default_xdt), use.names = TRUE, fill = TRUE)
      setorderv(initial_xdt, "branch.selection")

      # initialize mbo tuner
      tuner = tnr("adbo", initial_design = initial_xdt)

      # initialize auto tuner
      self$instance = TuningInstanceRushSingleCrit$new(
        task = task,
        learner = graph_learner,
        resampling = self$resampling,
        measure = self$measure,
        terminator = self$terminator,
        search_space = search_space,
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
LearnerClassifAuto = R6Class("LearnerClassifAuto",
  inherit = LearnerAuto,
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
      nthread = 1L,
      lhs_size = 4L,
      xgboost_eval_metric = NULL
      ) {
      assert_count(nthread)
      learner_ids = c("rpart", "glmnet", "kknn", "lda", "log_reg", "multinom", "naive_bayes", "nnet", "qda", "ranger", "svm", "xgboost")

      graph = po("removeconstants", id = "pre_removeconstants") %>>%
        po("branch", options = learner_ids) %>>%
        gunion(list(
          # rpart
          lrn("classif.rpart", id = "rpart"),
          # glmnet
          po("imputehist", id = "glmnet_imputehist") %>>% po("imputeoor", id = "glmnet_imputeoor") %>>% po("encode", method = "one-hot", id = "glmnet_encode") %>>% po("removeconstants", id = "glmnet_post_removeconstants") %>>% lrn("classif.glmnet", id = "glmnet"),
          # kknn
          po("imputehist", id = "kknn_imputehist") %>>% po("imputeoor", id = "kknn_imputeoor") %>>% po("removeconstants", id = "kknn_post_removeconstants") %>>% lrn("classif.kknn", id = "kknn"),
          # lda
          po("imputehist", id = "lda_imputehist") %>>% po("imputeoor", id = "lda_imputeoor") %>>% po("removeconstants", id = "lda_post_removeconstants") %>>% lrn("classif.lda", id = "lda"),
          # log_reg
          po("imputehist", id = "logreg_imputehist") %>>% po("imputeoor", id = "log_reg_imputeoor") %>>% po("removeconstants", id = "log_reg_post_removeconstants") %>>% lrn("classif.log_reg", id = "log_reg"),
          # multinom
          po("imputehist", id = "multinom_imputehist") %>>% po("imputeoor", id = "multinom_imputeoor") %>>% po("removeconstants", id = "multinom_post_removeconstants") %>>% lrn("classif.multinom", id = "multinom"),
          # naive_bayes
          po("imputehist", id = "naive_bayes_imputehist") %>>% po("imputeoor", id = "naive_bayes_imputeoor") %>>% po("removeconstants", id = "naive_bayes_post_removeconstants") %>>% lrn("classif.naive_bayes", id = "naive_bayes"),
          # nnet
          po("imputehist", id = "nnet_imputehist") %>>% po("imputeoor", id = "nnet_imputeoor") %>>% po("removeconstants", id = "nnet_post_removeconstants") %>>% lrn("classif.nnet", id = "nnet"),
          # qda
          po("imputehist", id = "qda_imputehist") %>>% po("imputeoor", id = "qda_imputeoor") %>>% po("removeconstants", id = "qda_post_removeconstants") %>>% lrn("classif.qda", id = "qda"),
          # ranger
          po("imputeoor", id = "ranger_imputeoor") %>>% po("removeconstants", id = "ranger_post_removeconstants") %>>% lrn("classif.ranger", id = "ranger", num.threads = nthread),
          # svm
          po("imputehist", id = "svm_imputehist") %>>% po("imputeoor", id = "svm_imputeoor") %>>% po("encode", method = "one-hot", id = "smv_encode") %>>% po("removeconstants", id = "svm_post_removeconstants") %>>% lrn("classif.svm", id = "svm", type = "C-classification"),
          # xgboost
          po("imputeoor", id = "xgboost_imputeoor") %>>% po("encode", method = "one-hot", id = "xgboost_encode") %>>% po("removeconstants", id = "xgboost_post_removeconstants") %>>% lrn("classif.xgboost", id = "xgboost", nrounds = 5000, early_stopping_rounds = 10, nthread = nthread)
        )) %>>% po("unbranch", options = learner_ids)

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
        lhs_size = lhs_size,
        xgboost_eval_metric = xgboost_eval_metric)
    }
  ),

  private = list(
    .train = function(task) {

      if ("twoclass" %in% task$properties) {
        self$learner_ids = self$learner_ids[self$learner_ids != "multinom"]
      } else {
        self$learner_ids = self$learner_ids[self$learner_ids != "log_reg"]
      }
      super$.train(task)
    }
  )
)

tuning_space = list(
  # glmnet
  glmnet.s     = to_tune(1e-4, 1e4, logscale = TRUE),
  glmnet.alpha = to_tune(0, 1),

  # kknn
  kknn.k = to_tune(1, 50, logscale = TRUE),
  kknn.distance = to_tune(1, 5),
  kknn.kernel = to_tune(c("rectangular", "optimal", "epanechnikov", "biweight", "triweight", "cos",  "inv",  "gaussian", "rank")),

  # nnet
  nnet.maxit = to_tune(1e1, 1e3, logscale = TRUE),
  nnet.decay = to_tune(1e-4, 1e-1, logscale = TRUE),
  nnet.size  = to_tune(2, 50, logscale = TRUE),

  # ranger
  ranger.mtry.ratio      = to_tune(0, 1),
  ranger.replace         = to_tune(),
  ranger.sample.fraction = to_tune(1e-1, 1),
  ranger.num.trees       = to_tune(500, 2000),

  # rpart
  rpart.minsplit  = to_tune(2, 128, logscale = TRUE),
  rpart.minbucket = to_tune(1, 64, logscale = TRUE),
  rpart.cp        = to_tune(1e-04, 1e-1, logscale = TRUE),

  # svm
  svm.cost    = to_tune(1e-4, 1e4, logscale = TRUE),
  svm.kernel  = to_tune(c("polynomial", "radial", "sigmoid", "linear")),
  svm.degree  = to_tune(2, 5),
  svm.gamma   = to_tune(1e-4, 1e4, logscale = TRUE),

  # xgboost
  xgboost.eta               = to_tune(1e-4, 1, logscale = TRUE),
  xgboost.max_depth         = to_tune(1, 20),
  xgboost.colsample_bytree  = to_tune(1e-1, 1),
  xgboost.colsample_bylevel = to_tune(1e-1, 1),
  xgboost.lambda            = to_tune(1e-3, 1e3, logscale = TRUE),
  xgboost.alpha             = to_tune(1e-3, 1e3, logscale = TRUE),
  xgboost.subsample         = to_tune(1e-1, 1)
)
