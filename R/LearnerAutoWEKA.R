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
        acq_optimizer = acq_optimizer,
        args = list(init_design_size = 10))

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

tuning_space_classif_autoweka = tuning_space_regr_autoweka = list(
  # J48
  J48.O     = to_tune(),
  J48.U     = to_tune(),
  J48.B     = to_tune(),
  J48.J     = to_tune(),
  J48.A     = to_tune(),
  J48.S     = to_tune(),
  J48.M     = to_tune(1, 64),
  J48.C     = to_tune(0, 1),

  # Decision Table
  DecisionTable.E     = to_tune(levels = c("acc", "auc")),
  DecisionTable.I     = to_tune(),
  DecisionTable.S     = to_tune(levels = c("BestFirst", "GreedyStepwise")),
  DecisionTable.X     = to_tune(1, 4),

  # KStar
  KStar.B     = to_tune(1, 100),
  KStar.E     = to_tune(),
  KStar.M     = to_tune(levels = c("a", "d", "m", "n")),

  # LMT
  LMT.B     = to_tune(),
  LMT.R     = to_tune(),
  LMT.C     = to_tune(),
  LMT.P     = to_tune(),
  LMT.M     = to_tune(1, 64),
  LMT.W     = to_tune(0, 1),
  LMT.A     = to_tune(),

  # PART
  PART.N     = to_tune(2, 5),
  PART.M     = to_tune(1, 64),
  PART.R     = to_tune(),
  PART.B     = to_tune(),

  # SMO
  # SMO.C       = to_tune(0.5, 1.5),
  # SMO.N       = to_tune(levels = c("0", "1", "2")),
  # SMO.M       = to_tune(),
  # SMO.K       = to_tune(levels = c("NormalizedPolyKernel", "PolyKernel", "Puk", "RBFKernel")),
  # SMO.E_poly  = to_tune(0.2, 5),
  # SMO.L_poly  = to_tune(),

  # BayesNet
  BayesNet.D   = to_tune(),
  BayesNet.Q   = to_tune(levels = c("local.K2", "local.HillClimber", "local.LAGDHillClimber",
                                    "local.SimulatedAnnealing", "local.TabuSearch", "local.TAN")),

  # JRip
  JRip.N   = to_tune(1, 5),
  JRip.E   = to_tune(),
  JRip.P   = to_tune(),
  JRip.O   = to_tune(1, 5),

  # SimpleLogistic
  SimpleLogistic.S   = to_tune(),
  SimpleLogistic.W   = to_tune(0, 1),
  SimpleLogistic.A   = to_tune(),

  # VotedPerceptron
  VotedPerceptron.I   = to_tune(1, 10),
  VotedPerceptron.M   = to_tune(5000, 50000),
  VotedPerceptron.E   = to_tune(0.2, 5),

  # SGD
  SGD.F   = to_tune(levels = c("0", "1")),
  SGD.L   = to_tune(0.00001, 0.1),
  SGD.R   = to_tune(1e-12, 10),
  SGD.N   = to_tune(),
  SGD.M   = to_tune(),

  # Logistic
  Logistic.R = to_tune(1e-12, 10),

  # OneR
  OneR.B = to_tune(1, 32),

  # MultilayerPerceptron
  MultilayerPerceptron.L   = to_tune(0.1, 1),
  MultilayerPerceptron.M   = to_tune(0.1, 1),
  MultilayerPerceptron.B   = to_tune(),
  MultilayerPerceptron.H   = to_tune(levels = c("a", "i", "o", "t")),
  MultilayerPerceptron.C   = to_tune(),
  MultilayerPerceptron.R   = to_tune(),
  MultilayerPerceptron.D   = to_tune(),
  MultilayerPerceptron.S   = to_tune(1, 1),

  # REPTree
  REPTree.M   = to_tune(1, 64),
  REPTree.V   = to_tune(1e-5, 1e-1),
  #FIXME: how to add both?
  #L   = to_tune(1, 1)
  REPTree.L   = to_tune(2, 20),
  REPTree.P   = to_tune(),

  # IBk
  IBk.E   = to_tune(),
  IBk.K   = to_tune(1, 64),
  IBk.X   = to_tune(),
  IBk.F   = to_tune(),
  IBk.I   = to_tune(),

  # RandomForestWEKA
  RandomForestWEKA.I       = to_tune(2, 256),
  RandomForestWEKA.K       = to_tune(0, 32),
  RandomForestWEKA.depth   = to_tune(0, 20),

  # RandomTree
  RandomTree.M       = to_tune(1, 64),
  #FIXME: K is 0 and 2-32
  RandomTree.K       = to_tune(0, 32),
  RandomTree.depth   = to_tune(0, 20),
  #FIXME: N is 0 and 2-5
  RandomTree.N       = to_tune(0, 5),
  RandomTree.U       = to_tune()
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

      learner_ids = c("J48", "DecisionTable", "KStar", "LMT", "PART", "BayesNet", "JRip", "SimpleLogistic",
        "VotedPerceptron", "SGD", "Logistic", "OneR", "MultilayerPerceptron", "REPTree", "IBk", "RandomForestWEKA",
        "RandomTree") # "SMO"

      super$initialize(
        id = id,
        task_type = "classif",
        learner_ids = learner_ids,
        tuning_space = tuning_space_classif_autoweka,
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

      learner_ids = c("DecisionTable", "GaussianProcesses", "M5P", "KStar", "LinearRegression", "SGD",
        "MultilayerPerceptron", "REPTree", "IBk", "M5Rules", "RandomForestWEKA", "RandomTree", "SMOreg")

      super$initialize(
        id = id,
        task_type = "regr",
        learner_ids = learner_ids,
        tuning_space = tuning_space_regr_autoweka,
        resampling = resampling,
        measure = measure,
        terminator = terminator,
        callbacks = callbacks)
    }
  )
)
