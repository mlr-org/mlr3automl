
test_that("default design is generated", {
  learner_ids = c("rpart", "glmnet", "kknn", "lda", "log_reg", "multinom", "naive_bayes", "nnet", "qda", "ranger", "svm", "xgboost")
  xdt = generate_default_design("classif", learner_ids, tsk("sonar"), tuning_space)
  expect_data_table(xdt, nrows = length(learner_ids))
})

test_that("lhs design is generated", {
  learner_ids = c("rpart", "glmnet", "kknn", "lda", "log_reg", "multinom", "naive_bayes", "nnet", "qda", "ranger", "svm", "xgboost")
  xdt = generate_lhs_design(10, "classif", learner_ids, tuning_space)
})

test_that("LearnerClassifAuto train works", {
  rush::rush_plan(n_workers = 4, lgr_thresholds = c(bbotk = "trace", rush = "debug", mlr3automl = "debug"))

  lgr::get_logger("bbotk")$set_threshold("trace")
  lgr::get_logger("rush")$set_threshold("debug")
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 100)
  learner = LearnerClassifAuto$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    lhs_size = 1)

  expect_class(learner$train(task), "LearnerClassifAuto")

  expect_class(learner$model, "AutoTuner")

  expect_prediction(learner$predict(task))
})

test_that("LearnerClassifAuto memory_limit works", {
  rush::rush_plan(n_workers = 4, lgr_thresholds = c(mlr3 = "info", bbotk = "trace", rush = "debug", mlr3automl = "debug"))

  lgr::get_logger("bbotk")$set_threshold("trace")
  lgr::get_logger("rush")$set_threshold("debug")
  lgr::get_logger("mlr3automl")$set_threshold("debug")
  lgr::get_logger("mlr3")$set_threshold("info")

  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 20)
  learner = LearnerClassifAuto$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    learner_memory_limit = 1e10)

  expect_class(learner$train(task), "LearnerClassifAuto")
}



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
