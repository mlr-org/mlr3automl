
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
LearnerClassifAutoSVM = R6Class("LearnerClassifAutoSVM",
  inherit = LearnerAuto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "classif.automl_svm",
      resampling = rsmp("holdout"),
      measure = msr("classif.ce"),
      terminator = trm("evals", n_evals = 100L),
      callbacks = list(),
      learner_timeout = Inf,
      learner_memory_limit = Inf,
      nthread = 1L,
      lhs_size = 4L,
      large_data_size = 1e6,
      large_data_nthread = 1L,
      small_data_size = 5000L,
      small_data_resampling = rsmp("cv", folds = 5),
      max_cardinality = 100L
      ) {
      assert_count(nthread)
      assert_count(max_cardinality)
      learner_id = "svm"

      # svm
      graph =
po("imputehist", id = "svm_imputehist") %>>%
        po("imputeoor", id = "svm_imputeoor") %>>%
        po("fixfactors", id = "svm_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "svm_imputesample") %>>%
        po("collapsefactors", target_level_count = max_cardinality, id = "svm_collapse") %>>%
        po("encode", method = "one-hot", id = "smv_encode") %>>%
        po("removeconstants", id = "svm_post_removeconstants") %>>%
        lrn("classif.svm", id = "svm", type = "C-classification")

      learner_fallback = lrn("classif.featureless", predict_type = measure$predict_type)

      super$initialize(
        id = id,
        task_type = "classif",
        learner_id = "svm",
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

      # initialize graph learner
      graph_learner = as_learner(self$graph)
      graph_learner$id = "graph_learner"
      graph_learner$predict_type = self$measure$predict_type
      graph_learner$fallback = self$learner_fallback
      graph_learner$encapsulate = c(train = "callr", predict = "callr")
      graph_learner$timeout = c(train = self$learner_timeout, predict = self$learner_timeout)
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
        callbacks = self$callbacks,
        store_benchmark_result = FALSE
      )

      # tune
      lg$debug("Learner '%s' starts tuning phase", self$id)
      tuner$optimize(self$instance)

      # fit final model
      lg$debug("Learner '%s' fits final model", self$id)
      graph_learner$param_set$set_values(.values = self$instance$result_learner_param_vals)
      graph_learner$timeout = c(train = Inf, predict = Inf)
      graph_learner$train(task)

      list(graph_learner = graph_learner, instance = self$instance)
    }
  )
)

tuning_space = list(
  svm = list(
    svm.cost    = to_tune(1e-4, 1e4, logscale = TRUE),
    svm.kernel  = to_tune(c("polynomial", "radial", "sigmoid", "linear")),
    svm.degree  = to_tune(2, 5),
    svm.gamma   = to_tune(1e-4, 1e4, logscale = TRUE)
  )
)
