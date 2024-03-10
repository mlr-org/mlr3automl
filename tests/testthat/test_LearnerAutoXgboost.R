test_that("LearnerClassifAutoBranch train works", {
  options(bbotk_local = TRUE)
  rush::rush_plan(n_workers = 4, lgr_thresholds = c(bbotk = "trace", rush = "debug", mlr3automl = "debug"))

  lgr::get_logger("bbotk")$set_threshold("trace")
  lgr::get_logger("rush")$set_threshold("debug")
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("evals", n_evals = 20)
  learner = LearnerClassifAutoXgboost$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    lhs_size = 10)

  expect_class(learner$train(task), "LearnerClassifAutoXGboost")

  expect_class(learner$model, "AutoTuner")

  expect_prediction(learner$predict(task))
})
