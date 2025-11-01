test_that("LearnerRegrAuto is initialized", {
  learner = lrn("regr.auto",
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 10))

  expect_learner(learner, task = tsk("california_housing"))
})

test_that("all learner on cpu work", {
  test_regr_learner(c("catboost", "glmnet", "lightgbm", "ranger", "svm", "xgboost", "extra_trees"), initial_design_type = "sobol", n_evals = 30)
})
