test_that("LearnerClassifAuto train works", {
  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 10)
  learner = LearnerClassifAuto$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  expect_names(learner$packages, must.include = "xgboost")

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_class(learner$model, "AutoTuner")

  expect_prediction(learner$predict(task))
})

test_that("LearnerClassifAuto resample works", {
  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 10)
  learner = LearnerClassifAuto$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  expect_resample_result(resample(task, learner, rsmp("holdout")))
})

test_that("LearnerClassifAuto train works with parallelization", {
  future::plan("multisession", workers = 2)

  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 10)
  learner = LearnerClassifAuto$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_class(learner$model, "AutoTuner")
})

test_that("callback timeout works", {
  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 20)
  learner = LearnerClassifAuto$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    callbacks = clbk("mlr3tuning.timeout", time_limit = 20))

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_lte(learner$model$tuning_instance$archive$benchmark_result$learners$learner[[1]]$timeout["train"], 20)
})


test_that("callback timeout works", {
  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 20)
  learner = LearnerClassifAuto$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    callbacks = clbk("mlr3tuning.timeout", time_limit = 20, max_time_limit = 5))

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_lte(learner$model$tuning_instance$archive$benchmark_result$learners$learner[[1]]$timeout["train"], 5)
})
