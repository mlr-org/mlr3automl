library(mlr3)
library(checkmate)
library(mlr3misc)
library(mlr3pipelines)
library(paradox)
library(R6)

lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)
lapply(list.files(system.file("testthat", package = "mlr3tuning"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

flush_redis = function() {
  config = redux::redis_config()
  r = redux::hiredis(config)
  r$FLUSHDB()
}

expect_rush_reset = function(rush, type = "kill") {
  rush$reset(type = type)
  Sys.sleep(1)
  keys = rush$connector$command(c("KEYS", "*"))
  if (!test_list(keys, len = 0)) {
    stopf("Found keys in redis after reset: %s", keys)
  }
  mirai::daemons(0)
}


test_classif_learner = function(
  learner_id,
  initial_design_size = 2,
  initial_design_type = "lhs",
  n_evals = 4,
  task = NULL,
  check_learners = TRUE
  ) {
  skip_on_cran()
  skip_if_not_installed(unlist(map(mlr_auto$mget(learner_id), "packages")))
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = if (is.null(task)) tsk("penguins") else task
  learner = lrn("classif.auto",
    learner_ids = learner_id,
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = n_evals),
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
  skip_on_cran()
  skip_if_not_installed(unlist(map(mlr_auto$mget(learner_id), "packages")))
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = if (is.null(task)) tsk("mtcars") else task
  learner = lrn("regr.auto",
    learner_ids = learner_id,
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = n_evals),
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

all_packages = c("glmnet", "kknn", "ranger", "e1071", "xgboost", "catboost","lightgbm", "fastai", "mlr3torch")
