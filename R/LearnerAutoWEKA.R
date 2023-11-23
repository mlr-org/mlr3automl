#' @title Auto-WEKA Learner
#'
#' @description
#' Abstract base class for Auto-WEKA like learner.
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#' @param task_type (`character(1)`)\cr
#'   Type of task, e.g. `"regr"` or `"classif"`.
#'   Must be an element of [mlr_reflections$task_types$type][mlr_reflections].
#' @param resampling ([mlr3::Resampling]).
#' @param measure ([mlr3::Measure]).
#' @param terminator ([bbotk::Terminator]).
#' @param callbacks (list of [mlr3tuning::CallbackTuning]).
#'
#' @export
LearnerAutoWEKA = R6Class("LearnerAutoWEKA",
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

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, task_type, resampling, measure, terminator, callbacks = list()) {
      self$resampling = assert_resampling(resampling)
      self$measure = assert_measure(measure)
      self$terminator = assert_terminator(terminator)
      self$callbacks = assert_list(as_callbacks(callbacks), types = "CallbackTuning")
      assert_choice(task_type, mlr_reflections$task_types$type)

      super$initialize(
        id = id,
        task_type = task_type,
        packages = c("mlr3tuning", "mlr3learners", "mlr3pipelines", "mlr3mbo", "mlr3automl", learners_default),
        feature_types = mlr_reflections$task_feature_types,
        predict_types = names(mlr_reflections$learner_predict_types[[task_type]]),
        properties = mlr_reflections$learner_properties[[task_type]],
      )
    }
  ),

  private = list(

    .train = function(task) {

      # initialize learners
      learner_names = paste(self$task_type, learners_default, sep = ".")
      learners = lrns(learner_names)

      set_threads(learners, n = 8)
      learners$classif.xgboost$param_set$set_values(nrounds = 50L)

      # initialize graph learner
      graph = ppl("robustify", task = task, factors_to_numeric = TRUE) %>>%
        ppl("branch", lapply(learners, po))
      graph_learner = as_learner(graph)
      graph_learner$id = "graph_learner"
      graph_learner$predict_type = self$measure$predict_type
      search_space = default_space(learners_default, self$task_type)
      graph_learner$fallback = switch(self$task_type,
        "classif" = lrn("classif.featureless", predict_type = self$measure$predict_type),
        "regr" = lrn("regr.featureless"))

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
        acq_optimizer = acq_optimizer,
        args = list(init_design_size = 20))

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

#' @title Classification Auto-WEKA Learner
#'
#' @description
#' Classification Auto-WEKA learner.
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#' @param resampling ([mlr3::Resampling]).
#' @param measure ([mlr3::Measure]).
#' @param terminator ([bbotk::Terminator]).
#' @param callbacks (list of [mlr3tuning::CallbackTuning]).
#'
#' @export
LearnerClassifAutoWEKA = R6Class("LearnerClassifAutoWEKA",
  inherit = LearnerAutoWEKA,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "classif.autoweka",
      resampling = rsmp("cv", folds = 3),
      measure = msr("classif.ce"),
      terminator = trm("evals", n_evals = 100L),
      callbacks = list()
      ){
      super$initialize(
        id = id,
        task_type = "classif",
        resampling = resampling,
        measure = measure,
        terminator = terminator,
        callbacks = callbacks)
    }
  )
)

#' @title Regression Auto-WEKA Learner
#'
#' @description
#' Regression Auto-WEKA learner.
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#' @param resampling ([mlr3::Resampling]).
#' @param measure ([mlr3::Measure]).
#' @param terminator ([bbotk::Terminator]).
#' @param callbacks (list of [mlr3tuning::CallbackTuning]).
#'
#' @export
LearnerRegrAutoWEKA = R6Class("LearnerRegrAutoWEKA",
  inherit = LearnerAutoWEKA,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "regr.autoweka",
      resampling = rsmp("cv", folds = 3),
      measure = msr("regr.rmse"),
      terminator = trm("evals", n_evals = 100L),
      callbacks = list()
      ){
      super$initialize(
        id = id,
        task_type = "regr",
        resampling = resampling,
        measure = measure,
        terminator = terminator,
        callbacks = callbacks)
    }
  )
)
