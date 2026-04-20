library(mlr3)
library(checkmate)
library(mlr3misc)
library(mlr3pipelines)
library(paradox)
library(R6)

lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)
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
  skip_if_not_all_installed(unlist(map(mlr_auto$mget(learner_id), "packages")))
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
  skip_if_not_all_installed(unlist(map(mlr_auto$mget(learner_id), "packages")))
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
