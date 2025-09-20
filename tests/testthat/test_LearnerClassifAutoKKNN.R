test_that("LearnerClassifAutoKKNN is initialized", {
  learner = lrn("classif.auto_kknn",
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10))

  expect_learner(learner)
})

test_that("design set is generated", {
  auto = mlr_auto$get("kknn")
  xdt = auto$design_set(tsk("penguins"), msr("classif.ce"), 10)
  expect_data_table(xdt, nrows = 10)
  expect_set_equal(xdt$branch.selection, "kknn")
  expect_set_equal(xdt$kknn.kernel, c("rectangular", "optimal", "epanechnikov", "biweight", "triweight", "cos",  "inv",  "gaussian", "rank"))

  xdt = auto$design_set(tsk("penguins"), msr("classif.ce"), 1)
  expect_data_table(xdt, nrows = 9)
  expect_set_equal(xdt$branch.selection, "kknn")
  expect_set_equal(xdt$kknn.kernel, c("rectangular", "optimal", "epanechnikov", "biweight", "triweight", "cos",  "inv",  "gaussian", "rank"))
})

test_that("LearnerClassifAutoKKNN is trained", {
  skip_on_cran()
  skip_if_not_installed("kknn")
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = tsk("penguins")
  learner = lrn("classif.auto_kknn",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10),
    initial_design_size = 1,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE
  )

  expect_class(learner$train(task), "LearnerClassifAutoKKNN")
  expect_equal(learner$model$instance$result$branch.selection, "kknn")
})
