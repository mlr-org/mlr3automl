test_that("LearnerClassifAutoWEKA train works", {
  task = tsk("sonar")
  resampling = rsmp("cv", folds = 3)
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 10)
  learner = LearnerClassifAutoWEKA$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  expect_names(learner$packages, must.include = "xgboost")

  expect_class(learner$train(task), "LearnerClassifAutoWEKA")
  expect_class(learner$model, "AutoTuner")

  expect_prediction(learner$predict(task))
})

test_that("LearnerClassifAutoWEKA resample works", {
  task = tsk("sonar")
  resampling = rsmp("cv", folds = 3)
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 10)
  learner = LearnerClassifAutoWEKA$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  expect_resample_result(resample(task, learner, rsmp("holdout")))
})

test_that("LearnerClassifAutoWEKA train works with parallelization", {
  future::plan("multisession", workers = 2)

  task = tsk("sonar")
  resampling = rsmp("cv", folds = 3)
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 10)
  learner = LearnerClassifAutoWEKA$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  expect_class(learner$train(task), "LearnerClassifAutoWEKA")
  expect_class(learner$model, "AutoTuner")
})

test_that("callback timeout works", {
  task = tsk("sonar")
  resampling = rsmp("cv", folds = 3)
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 20)
  learner = LearnerClassifAutoWEKA$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    callbacks = clbk("mlr3tuning.timeout", time_limit = 20))

  expect_class(learner$train(task), "LearnerClassifAutoWEKA")
  expect_lte(learner$model$tuning_instance$archive$benchmark_result$learners$learner[[1]]$timeout["train"], 20)
})


test_that("callback timeout works", {
  task = tsk("sonar")
  resampling = rsmp("cv", folds = 3)
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 20)
  learner = LearnerClassifAutoWEKA$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    callbacks = clbk("mlr3tuning.timeout", time_limit = 20, max_time_limit = 5))

  expect_class(learner$train(task), "LearnerClassifAutoWEKA")
  expect_lte(learner$model$tuning_instance$archive$benchmark_result$learners$learner[[1]]$timeout["train"], 5)
})
