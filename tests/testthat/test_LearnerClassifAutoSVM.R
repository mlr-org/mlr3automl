test_that("LearnerClassifAutoSVM works", {
  skip_on_cran()
  skip_if_not_installed(unlist(map(mlr_auto$mget("svm"), "packages")))
  skip_if_not_installed("rush")
  flush_redis()

  rush_plan(n_workers = 2, worker_type = "remote")
  mirai::daemons(2)

  task = tsk("penguins")
  learner = lrn("classif.auto_svm",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    initial_design_type = "lhs",
    initial_design_size = 2,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE,
    check_learners = TRUE
  )

  expect_class(learner$train(task), "LearnerClassifAutoSVM")
})


