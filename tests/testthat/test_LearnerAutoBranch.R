
test_that("initial design is generated", {
  learner_ids = c("glmnet", "kknn", "lda", "nnet", "ranger", "svm", "xgboost", "catboost")
  xdt = generate_default_design("classif", learner_ids, tsk("sonar"), tuning_space)
  expect_data_table(xdt, nrows = length(learner_ids))
})

test_that("lhs design is generated", {
  learner_ids = c("glmnet", "kknn", "lda", "nnet", "ranger", "svm", "xgboost", "catboost")
  xdt = generate_lhs_design(10, "classif", learner_ids, tuning_space)
})

test_that("LearnerClassifAutoBranch train works", {
  rush::rush_plan(n_workers = 4, lgr_thresholds = c(bbotk = "trace", rush = "debug", mlr3automl = "debug"), large_objects_path = ".")

  lgr::get_logger("bbotk")$set_threshold("trace")
  lgr::get_logger("rush")$set_threshold("debug")
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 20)
  learner = LearnerClassifAutoBranch$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    lhs_size = 1,
    small_data_resampling = rsmp("holdout"))

  expect_class(learner$train(task), "LearnerClassifAutoBranch")

  expect_class(learner$model, "AutoTuner")

  expect_prediction(learner$predict(task))
})

test_that("LearnerClassifAutoBranch works with large factors", {
  library(mlr3oml)

  rush::rush_plan(n_workers = 4, lgr_thresholds = c(bbotk = "trace", rush = "debug", mlr3automl = "debug"), large_objects_path = ".")

  lgr::get_logger("bbotk")$set_threshold("trace")
  lgr::get_logger("rush")$set_threshold("debug")
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  odata = odt(4135)
  task = as_task(odata)
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 20)
  learner = LearnerClassifAutoBranch$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    lhs_size = 1,
    max_cardinality = 100)

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
})

test_that("LearnerClassifAutoBranch works with small and large data sets", {
  rush::rush_plan(n_workers = 4, lgr_thresholds = c(bbotk = "trace", rush = "debug", mlr3automl = "debug"))

  lgr::get_logger("bbotk")$set_threshold("trace")
  lgr::get_logger("rush")$set_threshold("debug")
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 20)
  learner = LearnerClassifAutoBranch$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    lhs_size = 1,
    large_data_size = 300,
    large_data_nthread = 2,
    small_data_size = 1000L,
    small_data_resampling = rsmp("cv", folds = 5))

  expect_class(learner$train(task), "LearnerClassifAutoBranch")

  expect_class(learner$model, "AutoTuner")

  expect_prediction(learner$predict(task))
})

test_that("LearnerClassifAutoBranch works with large data sets", {
  rush::rush_plan(n_workers = 4, lgr_thresholds = c(bbotk = "trace", rush = "debug", mlr3automl = "debug"))

  lgr::get_logger("bbotk")$set_threshold("trace")
  lgr::get_logger("rush")$set_threshold("debug")
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 20)
  learner = LearnerClassifAutoBranch$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    lhs_size = 1,
    large_data_size = 300,
    large_data_nthread = 2)

  expect_class(learner$train(task), "LearnerClassifAutoBranch")

  expect_class(learner$model, "AutoTuner")

  expect_prediction(learner$predict(task))
})

test_that("LearnerClassifAutoBranch works with small data set resampling", {
  rush::rush_plan(n_workers = 4, lgr_thresholds = c(mlr3 = "info", bbotk = "trace", rush = "debug", mlr3automl = "debug"))

  lgr::get_logger("bbotk")$set_threshold("trace")
  lgr::get_logger("rush")$set_threshold("debug")
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 20)
  learner = LearnerClassifAutoBranch$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    lhs_size = 1,
    small_data_size = 1000L,
    small_data_resampling = rsmp("cv", folds = 5))

  expect_class(learner$train(task), "LearnerClassifAutoBranch")

  expect_class(learner$model, "AutoTuner")

  expect_prediction(learner$predict(task))
})

test_that("LearnerClassifAutoBranch memory_limit works", {
  rush::rush_plan(n_workers = 4, lgr_thresholds = c(mlr3 = "info", bbotk = "trace", rush = "debug", mlr3automl = "debug"))

  lgr::get_logger("bbotk")$set_threshold("trace")
  lgr::get_logger("rush")$set_threshold("debug")
  lgr::get_logger("mlr3automl")$set_threshold("debug")
  lgr::get_logger("mlr3")$set_threshold("info")

  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 20)
  learner = LearnerClassifAutoBranch$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    learner_memory_limit = 1e10)

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
}


test_that("LearnerClassifAutoBranch resample works", {
  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 10)
  learner = LearnerClassifAutoBranch$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  expect_resample_result(resample(task, learner, rsmp("holdout")))
})

test_that("LearnerClassifAutoBranch timeout works", {
  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 1)
  learner = LearnerClassifAutoBranch$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  expect_class(learner$train(task), "LearnerClassifAutoBranch")
  expect_class(learner$model, "AutoTuner")
  expect_equal(max(learner$model$tuning_instance$archive$data$batch_nr), 1)

  expect_prediction(learner$predict(task))
})




test_that("LearnerClassifAutoBranch nthread works", {
  task = tsk("sonar")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 1)
  learner = LearnerClassifAutoBranch$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    nthread = 2)

  expect_class(learner$train(task), "LearnerClassifAutoBranch")



  expect_prediction(learner$predict(task))
})
