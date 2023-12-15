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
#'
#' @export
LearnerAuto = R6Class("LearnerAuto",
  inherit = Learner,
  public = list(

    #' @field resampling ([mlr3::Resampling]).
    resampling = NULL,

    #' @field measure ([mlr3::Measure]).
    measure = NULL,

    #' @field terminator ([bbotk::Terminator]).
    terminator = NULL,

    #' @field callbacks (list of [mlr3tuning::CallbackTuning]).
    callbacks = NULL,

    #' @field learner_ids (`character()`).
    learner_ids = NULL,

    #' @field tuning_space (list of [TuneToken]).
    tuning_space = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, task_type, learner_ids, tuning_space, resampling, measure, terminator, callbacks = list()) {
      self$learner_ids = assert_character(learner_ids)
      self$tuning_space = assert_list(tuning_space, types = "TuneToken")
      self$resampling = assert_resampling(resampling)
      self$measure = assert_measure(measure)
      self$terminator = assert_terminator(terminator)
      self$callbacks = assert_list(as_callbacks(callbacks), types = "CallbackTuning")
      assert_choice(task_type, mlr_reflections$task_types$type)

      # find packages
      learners = lrns(paste0(task_type, ".", self$learner_ids))
      learner_packages = unlist(map(learners, "packages"))
      packages = unique(c("mlr3tuning", "mlr3learners", "mlr3pipelines", "mlr3mbo", "mlr3automl", learner_packages))

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
      # initialize graph learner
      gr_branch = get_branch_pipeline(self$task_type, self$learner_ids)
      graph = ppl("robustify", task = task, factors_to_numeric = TRUE) %>>% gr_branch
      graph_learner = as_learner(graph)
      graph_learner$id = "graph_learner"
      graph_learner$predict_type = self$measure$predict_type
      graph_learner$fallback = switch(self$task_type,
        "classif" = lrn("classif.featureless", predict_type = self$measure$predict_type),
        "regr" = lrn("regr.featureless"))
      graph_learner$encapsulate = c(train = "callr", predict = "callr")

      # initialize search space
      search_space = get_search_space(self$task_type, self$learner_ids, self$tuning_space)

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

      # initialize auto tuner
      auto_tuner = auto_tuner(
        tuner = tuner,
        learner = graph_learner,
        resampling = self$resampling,
        measure = self$measure,
        terminator = self$terminator,
        search_space = search_space,
        callbacks = self$callbacks
      )

      auto_tuner$train(task)
      auto_tuner
    },

    .predict = function(task) {
      self$model$predict(task)
    }
  )
)

tuning_space_auto = list(
  # glmnet
  glmnet.s     = to_tune(1e-4, 1e4, logscale = TRUE),
  glmnet.alpha = to_tune(0, 1),

  # kknn
  kknn.k        = to_tune(1, 50, logscale = TRUE),
  kknn.distance = to_tune(1, 5),
  kknn.kernel   = to_tune(c("rectangular", "optimal", "epanechnikov", "biweight", "triweight", "cos",  "inv",  "gaussian", "rank")),

  # ranger
  ranger.mtry.ratio      = to_tune(0, 1),
  ranger.replace         = to_tune(),
  ranger.sample.fraction = to_tune(1e-1, 1),

  # rpart
  rpart.minsplit  = to_tune(2, 128, logscale = TRUE),
  rpart.minbucket = to_tune(1, 64, logscale = TRUE),
  rpart.cp        = to_tune(1e-04, 1e-1, logscale = TRUE),

  # xgboost
  xgboost.eta               = to_tune(1e-4, 1, logscale = TRUE),
  xgboost.max_depth         = to_tune(1, 20),
  xgboost.colsample_bytree  = to_tune(1e-1, 1),
  xgboost.colsample_bylevel = to_tune(1e-1, 1),
  xgboost.lambda            = to_tune(1e-3, 1e3, logscale = TRUE),
  xgboost.alpha             = to_tune(1e-3, 1e3, logscale = TRUE),
  xgboost.subsample         = to_tune(1e-1, 1)
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
#'
#' @export
LearnerClassifAuto = R6Class("LearnerClassifAuto",
  inherit = LearnerAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "classif.auto",
      resampling = rsmp("cv", folds = 3),
      measure = msr("classif.ce"),
      terminator = trm("evals", n_evals = 100L),
      callbacks = list()
      ){

      learner_ids = c("rpart", "ranger", "xgboost", "glmnet", "kknn")

      super$initialize(
        id = id,
        task_type = "classif",
        learner_ids = learner_ids,
        tuning_space = tuning_space_auto,
        resampling = resampling,
        measure = measure,
        terminator = terminator,
        callbacks = callbacks)
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
#'
#' @export
LearnerRegrAuto = R6Class("LearnerRegrAuto",
  inherit = LearnerAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "regr.auto",
      resampling = rsmp("cv", folds = 3),
      measure = msr("regr.rmse"),
      terminator = trm("evals", n_evals = 100L),
      callbacks = list()
      ){

      learner_ids = c("rpart", "ranger", "xgboost", "glmnet", "kknn")

      super$initialize(
        id = id,
        task_type = "regr",
        learner_ids = learner_ids,
        tuning_space = tuning_space_auto,
        resampling = resampling,
        measure = measure,
        terminator = terminator,
        callbacks = callbacks)
    }
  )
)
