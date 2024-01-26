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
#' @param tuning_space (list of [paradox::TuneToken])\cr
#'  List of [paradox::TuneToken]s.
#' @param resampling ([mlr3::Resampling]).
#' @param measure ([mlr3::Measure]).
#' @param terminator ([bbotk::Terminator]).
#' @param callbacks (list of [mlr3tuning::CallbackTuning]).
#' @param learner_fallback ([mlr3::Learner]).
#' @param learner_timeout (`integer(1)`).
#' @param nrounds (`integer(1)`).
#' @param early_stopping_rounds (`integer(1)`).
#' @param early_stopping_nrounds (`integer(1)`).
#' @param nthread (`integer(1)`).
#' @param eval_metric (`character(1)`).
#'
#' @export
LearnerAuto = R6Class("LearnerAuto",
  inherit = Learner,
  public = list(

    #' @field learner_ids (`character()`).
    learner_ids = NULL,

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

    #' @field learner_timeout (`integer(1)`).
    learner_timeout = NULL,

    #' @field learner_fallback ([mlr3::Learner]).
    learner_fallback = NULL,

    #' @field nrounds (`integer(1)`).
    nrounds = NULL,

    #' @field early_stopping_nrounds (`integer(1)`).
    early_stopping_rounds = NULL,

    #' @field early_stopping_nrounds (`integer(1)`).
    early_stopping_nrounds = NULL,

    #' @field nthread (`integer(1)`).
    nthread = NULL,

    #' @field eval_metric (`character(1)`).
    eval_metric = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id,
      task_type,
      learner_ids,
      tuning_space,
      resampling,
      measure,
      terminator,
      callbacks = list(),
      learner_timeout = Inf,
      learner_fallback = NULL,
      nrounds = 10,
      early_stopping_rounds = 10,
      early_stopping_nrounds = 1000,
      nthread = 1,
      eval_metric = NULL
      ) {
      assert_choice(task_type, mlr_reflections$task_types$type)
      self$learner_ids = assert_character(learner_ids)
      self$tuning_space = assert_list(tuning_space, types = "TuneToken")
      self$resampling = assert_resampling(resampling)
      self$measure = assert_measure(measure)
      self$terminator = assert_terminator(terminator)
      self$callbacks = assert_list(as_callbacks(callbacks), types = "CallbackTuning")
      self$learner_timeout = assert_numeric(learner_timeout)
      self$learner_fallback = assert_learner(learner_fallback)
      self$nrounds = assert_int(nrounds)
      self$early_stopping_rounds = assert_int(early_stopping_rounds)
      self$early_stopping_nrounds = assert_int(early_stopping_nrounds)
      self$nthread = assert_int(nthread)
      self$eval_metric = assert_character(eval_metric, null.ok = TRUE)

      super$initialize(
        id = id,
        task_type = task_type,
        packages = c("mlr3tuning", "mlr3learners", "mlr3pipelines", "mlr3mbo", "mlr3automl", "xgboost"),
        feature_types = mlr_reflections$task_feature_types,
        predict_types = names(mlr_reflections$learner_predict_types[[task_type]]),
        properties = mlr_reflections$learner_properties[[task_type]],
      )
    }
  ),

  private = list(

    .train = function(task) {

      if (is.null(task$row_roles$holdout)) {
        stopf("No holdout set found.")
      }

      learner = lrn(sprintf("%s.xgboost", self$task_type), id = "xgboost", nthread = self$nthread, nrounds = self$nrounds)
      preproc = ppl("robustify", task = task, learner = learner)
      graph_learner = as_learner(preproc %>>% learner)
      graph_learner$param_set$set_values(.values = tuning_space)
      graph_learner$predict_type = self$measure$predict_type
      graph_learner$fallback = self$learner_fallback
      graph_learner$encapsulate = c(train = "callr", predict = "callr")
      graph_learner$timeout = c(train = self$learner_timeout, predict = self$learner_timeout)
      graph_learner$id = "graph_learner"

      search_space = graph_learner$param_set$search_space()

      instance = ti(
        task = task,
        learner = graph_learner,
        resampling = self$resampling,
        measure = self$measure,
        terminator = self$terminator,
        callbacks = self$callbacks,
      )

      # evaluate initial design
      initial_xdt = generate_design_lhs(search_space, 20)$data
      instance$eval_batch(initial_xdt)

      # initialize mbo tuner
      surrogate = default_surrogate(n_learner = 1, search_space = search_space, noisy = TRUE)
      acq_function = AcqFunctionEI$new()
      acq_optimizer = AcqOptimizer$new(
        optimizer = opt("random_search", batch_size = 1000L),
        terminator = trm("evals", n_evals = 10000L))
      tuner = tnr("mbo",
        loop_function = bayesopt_ego,
        surrogate = surrogate,
        acq_function = acq_function,
        acq_optimizer = acq_optimizer)

      tuner$optimize(instance)

      graph_learner$param_set$values = instance$result_learner_param_vals
      graph_learner$param_set$values$xgboost.early_stopping_rounds = self$early_stopping_rounds
      graph_learner$param_set$values$xgboost.nrounds = self$early_stopping_nrounds
      graph_learner$param_set$values$xgboost.eval_metric = self$eval_metric
      # pointer to early stopping set breaks with callr
      graph_learner$encapsulate = c(train = "none", predict = "none")
      graph_learner$timeout = c(train = Inf, predict = Inf)

      # prepare early stopping set
      early_stopping_task = task$clone()
      early_stopping_task$filter(early_stopping_task$row_roles$holdout)
      early_stopping_task$set_row_roles(early_stopping_task$row_roles$holdout, "use")
      early_stopping_task = preproc$train(early_stopping_task)[[1]]
      early_stopping_data = early_stopping_task$data(cols = early_stopping_task$feature_names)
      early_stopping_label = length(early_stopping_task$class_names) - as.integer(early_stopping_task$truth())
      early_stopping_data = xgboost::xgb.DMatrix(data = mlr3learners:::as_numeric_matrix(early_stopping_data), label = early_stopping_label)
      graph_learner$param_set$values$xgboost.watchlist = list(holdout = early_stopping_data)

      graph_learner$train(task)

      list(graph_learner = graph_learner, instance = instance)
    },

    .predict = function(task) {
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
#' @param learner_fallback ([mlr3::Learner]).
#' @param learner_timeout (`integer(1)`).
#' @param nrounds (`integer(1)`).
#' @param early_stopping_rounds (`integer(1)`).
#' @param early_stopping_nrounds (`integer(1)`).
#' @param nthread (`integer(1)`).
#' @param eval_metric (`character(1)`).
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
      learner_fallback = NULL,
      nrounds = 10,
      early_stopping_rounds = 10,
      early_stopping_nrounds = 1000,
      nthread = 1,
      eval_metric = NULL
      ){

      learner_ids = "xgboost"
      learner_fallback = lrn("classif.featureless", predict_type = measure$predict_type)

      super$initialize(
        id = id,
        task_type = "classif",
        learner_ids = learner_ids,
        tuning_space = tuning_space,
        resampling = resampling,
        measure = measure,
        terminator = terminator,
        callbacks = callbacks,
        learner_timeout = learner_timeout,
        learner_fallback = learner_fallback,
        nrounds = nrounds,
        early_stopping_rounds,
        early_stopping_nrounds = early_stopping_nrounds,
        nthread = nthread,
        eval_metric = eval_metric)
    }
  )
)

#' @title Regression Auto Learner
#'
#' @description
#' Regression Auto learner.
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#' @param resampling ([mlr3::Resampling]).
#' @param measure ([mlr3::Measure]).
#' @param terminator ([bbotk::Terminator]).
#' @param callbacks (list of [mlr3tuning::CallbackTuning]).
#' @param learner_fallback ([mlr3::Learner]).
#' @param learner_timeout (`integer(1)`).
#' @param nrounds (`integer(1)`).
#' @param early_stopping_nrounds (`integer(1)`).
#' @param nthread (`integer(1)`).
#' @param eval_metric (`character(1)`).
#'
#' @export
LearnerRegrAuto = R6Class("LearnerRegrAuto",
  inherit = LearnerAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "regr.automl",
      resampling = rsmp("holdout"),
      measure = msr("regr.rmse"),
      terminator = trm("evals", n_evals = 100L),
      callbacks = list(),
      learner_timeout = Inf,
      learner_fallback = NULL,
      nrounds = 10,
      early_stopping_rounds = 10,
      early_stopping_nrounds = 1000,
      nthread = 1,
      eval_metric = NULL
      ){

      learner_ids = "xgboost"
      learner_fallback = lrn("regr.featureless")

      super$initialize(
        id = id,
        task_type = "regr",
        learner_ids = learner_ids,
        tuning_space = tuning_space,
        resampling = resampling,
        measure = measure,
        terminator = terminator,
        callbacks = callbacks,
        learner_timeout = learner_timeout,
        learner_fallback = learner_fallback,
        nrounds = nrounds,
        early_stopping_rounds = early_stopping_rounds,
        early_stopping_nrounds = early_stopping_nrounds,
        nthread = nthread,
        eval_metric = eval_metric)
    }
  )
)

tuning_space = list(
  # xgboost
  xgboost.eta               = to_tune(1e-4, 1, logscale = TRUE),
  xgboost.max_depth         = to_tune(1, 20),
  xgboost.colsample_bytree  = to_tune(1e-1, 1),
  xgboost.colsample_bylevel = to_tune(1e-1, 1),
  xgboost.lambda            = to_tune(1e-3, 1e3, logscale = TRUE),
  xgboost.alpha             = to_tune(1e-3, 1e3, logscale = TRUE),
  xgboost.subsample         = to_tune(1e-1, 1)
)
