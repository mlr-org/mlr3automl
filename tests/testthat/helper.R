library(mlr3)
library(checkmate)
library(mlr3misc)
library(mlr3pipelines)
library(paradox)
library(R6)

lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)
lapply(list.files(system.file("testthat", package = "mlr3tuning"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

test_classif_learner = function(learner_id, n_evals = 6) {
  skip_on_cran()
  skip_if_not_installed(learner_id)
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto",
    learner_ids = learner_id,
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = n_evals),
    lhs_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_equal(learner$graph$param_set$values$branch.selection, learner_id)
  expect_equal(learner$model$instance$result$branch.selection, learner_id)

  learner
}

test_regr_learner = function(learner_id, n_evals = 6) {
  skip_on_cran()
  skip_if_not_installed(learner_id)
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  task = tsk("mtcars")
  learner = lrn("regr.auto",
    learner_ids = learner_id,
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = n_evals),
    lhs_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_equal(learner$graph$param_set$values$branch.selection, learner_id)
  expect_equal(learner$model$instance$result$branch.selection, learner_id)

  learner
}

