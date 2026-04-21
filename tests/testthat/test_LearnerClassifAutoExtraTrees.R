test_that("LearnerClassifAutoExtraTrees works", {
  skip_on_cran()
  skip_if_not_all_installed(unlist(map(mlr_auto$mget("extra_trees"), "packages")))
  skip_if_not_installed("rush")
  skip_if_no_redis()

  task = tsk("penguins")
  expect_error(
    lrn(
      "classif.auto_extra_trees",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr("classif.ce"),
      terminator = trm("evals", n_evals = 4)
    )$train(task),
    "All learners have no hyperparameters to tune"
  )
})
