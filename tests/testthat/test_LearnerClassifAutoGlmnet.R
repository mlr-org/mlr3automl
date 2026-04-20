test_that("LearnerClassifAutoGlmnet works", {
  skip_on_cran()
  skip_if_not_installed("glmnet")
  skip_if_not_installed("rush")
  skip_if_no_redis()

  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  task = tsk("penguins")
  learner = lrn("classif.auto_glmnet",
    rush = rush,
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

  expect_class(learner$train(task), "LearnerClassifAutoGlmnet")
})
