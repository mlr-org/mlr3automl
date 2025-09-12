test_that("LearnerClassifAutoKKNN is initialized", {
  learner = lrn("classif.auto_kknn",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("LearnerClassifAutoKKNN is trained", {
  skip_on_cran()
  skip_if_not_installed("kknn")
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)

  task = tsk("penguins")
  learner = lrn("classif.auto_kknn",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10),
    lhs_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAutoKKNN")
  expect_equal(learner$graph$param_set$values$branch.selection, "kknn")
  expect_equal(learner$model$instance$result$branch.selection, "kknn")
})
