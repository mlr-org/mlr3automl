test_that("LearnerClassifAutoRanger is initialized", {
  learner = lrn("classif.auto_ranger",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_class(learner$graph, "Graph")
  expect_list(learner$tuning_space)
})

test_that("LearnerClassifAutoRanger is trained", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2)


  task = tsk("penguins")
  learner = lrn("classif.auto_ranger",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoRanger")
  expect_equal(learner$graph$param_set$values$branch.selection, "ranger")
  expect_equal(learner$model$instance$result$branch.selection, "ranger")
})
