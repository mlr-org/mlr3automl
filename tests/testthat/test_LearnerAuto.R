test_that("LearnerClassifAuto train works", {
  task = tsk("penguins")
  splits = partition(task, ratio = 0.9, stratify = TRUE)
  task$set_row_roles(splits$test, "holdout")

  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 10)
  learner = LearnerClassifAuto$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_class(learner$model$graph_learner, "GraphLearner")
  expect_class(learner$model$instance, "TuningInstanceSingleCrit")
  expect_class(learner$model$graph_learner$state$model$xgboost$model, "xgb.Booster")
  expect_prediction(learner$predict(task))
})

test_that("LearnerClassifAuto train works with early stopping eval metric", {
  task = tsk("pima")
  splits = partition(task, ratio = 0.9, stratify = TRUE)
  task$set_row_roles(splits$test, "holdout")

  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 10)
  learner = LearnerClassifAuto$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator,
    eval_metric = "auc")

  expect_class(learner$train(task), "LearnerClassifAuto")
  expect_class(learner$model$graph_learner, "GraphLearner")
  expect_class(learner$model$instance, "TuningInstanceSingleCrit")
  expect_class(learner$model$graph_learner$state$model$xgboost$model, "xgb.Booster")
  expect_true(grepl("auc", learner$model$graph_learner$model$xgboost$model$best_msg))
  expect_prediction(learner$predict(task))
})

test_that("LearnerClassifAuto works with resample", {
  task = tsk("penguins")
  splits = partition(task, ratio = 0.9, stratify = TRUE)
  task$set_row_roles(splits$test, "holdout")

  resampling = rsmp("holdout")
  measure = msr("classif.ce")
  terminator = trm("run_time", secs = 10)
  learner = LearnerClassifAuto$new(
    resampling = resampling,
    measure = measure,
    terminator = terminator)

  rr = resample(task, learner, rsmp("cv", folds = 3), store_models = TRUE)
  expect_resample_result(rr)

  expect_class(rr$learners[[1]]$model$graph_learner, "GraphLearner")
  expect_class(rr$learners[[1]]$model$graph_learner$state$model$xgboost$model, "xgb.Booster")
  expect_class(rr$learners[[1]]$model$instance, "TuningInstanceSingleCrit")
})
