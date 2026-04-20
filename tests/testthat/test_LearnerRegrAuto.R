test_that("LearnerRegrAuto is initialized", {
  learner = lrn("regr.auto",
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 10))

  expect_learner(learner, task = tsk("california_housing"))
})

test_that("all learner on cpu work", {
  skip_on_cran()
  skip_if_not_all_installed(unlist(map(mlr_auto$mget(c("catboost", "glmnet", "lightgbm", "ranger", "svm", "xgboost", "extra_trees")), "packages")))
  skip_if_not_installed("rush")
  skip_if_no_redis()

  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  task = tsk("mtcars")
  learner = lrn("regr.auto",
    rush = rush,
    learner_ids = c("catboost", "glmnet", "lightgbm", "ranger", "svm", "xgboost", "extra_trees"),
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("regr.rmse"),
    terminator = trm("evals", n_evals = 30),
    initial_design_size = 30,
    initial_design_type = "sobol",
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE,
    check_learners = TRUE
  )

  expect_class(learner$train(task), "LearnerRegrAuto")
  expect_set_equal(learner$model$instance$archive$data$branch.selection, c("catboost", "glmnet", "lightgbm", "ranger", "svm", "xgboost", "extra_trees"))
})
