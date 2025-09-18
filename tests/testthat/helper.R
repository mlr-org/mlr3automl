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


test_classif_learner = function(learner_id, n_evals = 6) {
  skip_on_cran()
  skip_if_not_installed(unlist(map(mlr_auto$mget(learner_id), "packages")))
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

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
  expect_subset(learner$model$instance$result$branch.selection, learner_id)
  expect_set_equal(learner$model$instance$archive$data$branch.selection, learner_id)

  learner
}

test_regr_learner = function(learner_id, n_evals = 6) {
  skip_on_cran()
   if ("extra_trees" %in% learner_id) learner_id = c(learner_id, "ranger")
  skip_if_not_installed(setdiff(learner_id, c("lda", "extra_trees")))
  skip_if_not_installed(learner_id)
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = tsk("california_housing")$filter(sample(1000))
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
  expect_subset(learner$model$instance$result$branch.selection, learner_id)
  expect_set_equal(learner$model$instance$archive$data$branch.selection, learner_id)

  learner
}

