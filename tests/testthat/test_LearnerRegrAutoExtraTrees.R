test_that("LearnerRegrAutoExtraTrees works", {
  skip_on_cran()
  skip_if_not_all_installed(unlist(map(mlr_auto$mget("extra_trees"), "packages")))
  skip_if_not_installed("rush")
  skip_if_no_redis()

  task = tsk("mtcars")
  expect_error(
    lrn(
      "regr.auto_extra_trees",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr("regr.rmse"),
      terminator = trm("evals", n_evals = 4)
    )$train(task),
    "All learners have no hyperparameters to tune"
  )
})
