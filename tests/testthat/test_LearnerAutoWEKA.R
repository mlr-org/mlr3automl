test_that("LearnerClassifAutoWEKA train works", {
  task = tsk("sonar")
  resampling = rsmp("cv", folds = 3)
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 30)
  learner = LearnerClassifAutoWEKA$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  learner$train(task)
})

test_that("LearnerClassifAutoWEKA resample works", {
  task = tsk("sonar")
  resampling = rsmp("cv", folds = 3)
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 30)
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
  terminator = trm("run_time", secs = 30)
  learner = LearnerClassifAutoWEKA$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  learner$train(task)
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

  learner$train(task)

  expect_true(learner$model$tuning_instance$archive$benchmark_result$learners$learner[[1]]$timeout["train"] < 20)
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

  learner$train(task)

  expect_equal(learner$model$tuning_instance$archive$benchmark_result$learners$learner[[1]]$timeout["train"], c(train = 5))
})
