
test_that("initial design is generated", {
  learner_ids = c("rpart", "glmnet", "kknn", "lda", "log_reg", "multinom", "naive_bayes", "nnet", "qda", "ranger", "svm", "xgboost")
  xdt = generate_initial_design("classif", learner_ids, tsk("sonar"), tuning_space)
  expect_data_table(xdt, nrows = length(learner_ids))
})

test_that("LearnerClassifAuto train works", {
  task = tsk("penguins")
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

test_that("LearnerClassifAuto timeout works", {
  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 1)
  learner = LearnerClassifAuto$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_class(learner$model, "AutoTuner")
  expect_equal(max(learner$model$tuning_instance$archive$data$batch_nr), 1)

  expect_prediction(learner$predict(task))
})

test_that("LearnerClassifAuto nthread works", {
  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 1)
  learner = LearnerClassifAuto$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    nthread = 2)

  expect_class(learner$train(task), "LearnerClassifAuto")



  expect_prediction(learner$predict(task))
})
