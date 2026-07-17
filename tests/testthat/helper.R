library(mlr3)
library(checkmate)
library(mlr3misc)
library(mlr3pipelines)
library(paradox)
library(R6)

mlr3_helpers = list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE)
# helper_debugging.R defines strict $.R6 and [[.R6 methods that error on missing slots.
# these are incompatible with torch's R6 classes (e.g. the dataloader probes optional
# fields expecting NULL), which the mlp, resnet and ft_transformer learners use.
mlr3_helpers = grep("helper_debugging", mlr3_helpers, value = TRUE, invert = TRUE)
lapply(mlr3_helpers, source)
lapply(
  list.files(system.file("testthat", package = "mlr3tuning"), pattern = "^helper.*\\.[rR]", full.names = TRUE),
  source
)
lapply(list.files(system.file("testthat", package = "rush"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

skip_if_not_all_installed = function(pkgs) {
  for (pkg in pkgs) {
    testthat::skip_if_not_installed(pkg)
  }
}

skip_if_auto_not_installed = function(auto_id) {
  skip_if_not_all_installed(unlist(map(mlr_auto$mget(auto_id), "packages")))
}

test_classif_learner = function(
  learner_id,
  initial_design_size = 2,
  initial_design_type = "lhs",
  n_evals = 4,
  task = NULL,
  check_learners = TRUE
) {
  testthat::skip_on_cran()
  # nolint next: object_usage_linter
  skip_if_auto_not_installed(learner_id)
  testthat::skip_if_not_installed("rush")
  # nolint next: object_usage_linter
  skip_if_no_redis()

  # nolint next: object_usage_linter
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  task = if (is.null(task)) tsk("penguins") else task
  learner = lrn(
    "classif.auto",
    learner_ids = learner_id,
    rush = rush,
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = bbotk::trm("evals", n_evals = n_evals),
    initial_design_type = initial_design_type,
    initial_design_size = initial_design_size,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE,
    check_learners = check_learners
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_subset(learner$model$instance$result$branch.selection, learner_id)
  expect_set_equal(learner$model$instance$archive$data$branch.selection, learner_id)

  learner
}

test_regr_learner = function(
  learner_id,
  initial_design_size = 2,
  initial_design_type = "lhs",
  n_evals = 4,
  task = NULL,
  check_learners = TRUE
) {
  testthat::skip_on_cran()
  # nolint next: object_usage_linter
  skip_if_auto_not_installed(learner_id)
  testthat::skip_if_not_installed("rush")
  # nolint next: object_usage_linter
  skip_if_no_redis()

  # nolint next: object_usage_linter
  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  task = if (is.null(task)) tsk("mtcars") else task
  learner = lrn(
    "regr.auto",
    learner_ids = learner_id,
    rush = rush,
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("regr.rmse"),
    terminator = bbotk::trm("evals", n_evals = n_evals),
    initial_design_type = initial_design_type,
    initial_design_size = initial_design_size,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE,
    check_learners = check_learners
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_subset(learner$model$instance$result$branch.selection, learner_id)
  expect_set_equal(learner$model$instance$archive$data$branch.selection, learner_id)

  learner
}

all_packages = c("glmnet", "kknn", "ranger", "e1071", "xgboost", "catboost", "lightgbm", "fastai", "mlr3torch")

AutoDebug = R6Class(
  "AutoDebug",
  inherit = Auto,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' The probability arguments are passed to the underlying [mlr3::mlr_learners_classif.debug] learner and control the
    #' behavior during the tuning phase.
    #' `error_final_train` fails the final model fit that follows tuning, independently of the tuning phase.
    #'
    #' @param id (`character(1)`).
    #' @param error_train (`numeric(1)`)\cr
    #'   Probability to raise an error during training in the tuning phase.
    #' @param error_predict (`numeric(1)`)\cr
    #'   Probability to raise an error during prediction in the tuning phase.
    #' @param warning_train (`numeric(1)`)\cr
    #'   Probability to signal a warning during training in the tuning phase.
    #' @param warning_predict (`numeric(1)`)\cr
    #'   Probability to signal a warning during prediction in the tuning phase.
    #' @param message_train (`numeric(1)`)\cr
    #'   Probability to signal a message during training in the tuning phase.
    #' @param message_predict (`numeric(1)`)\cr
    #'   Probability to signal a message during prediction in the tuning phase.
    #' @param segfault_train (`numeric(1)`)\cr
    #'   Probability to provoke a segfault during training in the tuning phase.
    #' @param segfault_predict (`numeric(1)`)\cr
    #'   Probability to provoke a segfault during prediction in the tuning phase.
    #' @param error_final_train (`logical(1)`)\cr
    #'   Whether to fail the final model fit that is trained on the full task after tuning.
    initialize = function(
      id = "debug",
      error_train = 0,
      error_predict = 0,
      warning_train = 0,
      warning_predict = 0,
      message_train = 0,
      message_predict = 0,
      segfault_train = 0,
      segfault_predict = 0,
      error_final_train = FALSE
    ) {
      private$.debug_values = list(
        error_train = assert_number(error_train, lower = 0, upper = 1),
        error_predict = assert_number(error_predict, lower = 0, upper = 1),
        warning_train = assert_number(warning_train, lower = 0, upper = 1),
        warning_predict = assert_number(warning_predict, lower = 0, upper = 1),
        message_train = assert_number(message_train, lower = 0, upper = 1),
        message_predict = assert_number(message_predict, lower = 0, upper = 1),
        segfault_train = assert_number(segfault_train, lower = 0, upper = 1),
        segfault_predict = assert_number(segfault_predict, lower = 0, upper = 1)
      )
      private$.error_final_train = assert_flag(error_final_train)

      super$initialize(
        id = id,
        task_types = "classif",
        packages = "mlr3",
        devices = "cpu"
      )
    },

    #' @description
    #' Create the graph for the debug auto.
    #'
    #' @return [mlr3pipelines::Graph].
    graph = function(task, measure, n_threads, timeout, devices) {
      learner = lrn("classif.debug", id = "debug")
      learner$param_set$set_values(.values = private$.debug_values)
      po("removeconstants", id = "debug_removeconstants") %>>% learner
    },

    #' @description
    #' Prepare the graph learner for the final model fit.
    #' Fails the final fit when `error_final_train` is set to `TRUE`.
    #'
    #' @param graph_learner ([mlr3pipelines::GraphLearner]).
    #' @return ([mlr3pipelines::GraphLearner]).
    finalize_model = function(graph_learner) {
      if (private$.error_final_train) {
        graph_learner$param_set$set_values(debug.error_train = 1)
      }
      invisible(graph_learner)
    },

    #' @description
    #' Get the search space for the debug auto.
    #'
    #' @return [paradox::ParamSet].
    search_space = function(task) {
      ps(debug.x = p_dbl(0, 1))
    }
  ),
  private = list(
    .default_values = list(debug.x = 0.5),
    .debug_values = NULL,
    .error_final_train = FALSE
  )
)
