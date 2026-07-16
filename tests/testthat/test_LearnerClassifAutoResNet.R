test_that("LearnerClassifAutoResNet works", {
  skip_on_cran()
  skip_if_auto_not_installed("resnet")
  skip_if_not_installed("rush")
  skip_if_no_redis()
  skip_if(!torch::torch_is_installed(), "torch backend (LibTorch) not installed")

  rush = start_rush()
  on.exit({
    rush$reset()
    mirai::daemons(0)
  })

  task = tsk("penguins")
  task$filter(c(1, 153, 277))

  learner = lrn(
    "classif.auto_resnet",
    small_data_size = 1,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    initial_design_type = "lhs",
    initial_design_size = 2,
    encapsulate_learner = FALSE,
    encapsulate_mbo = FALSE,
    check_learners = FALSE,
    rush = rush
  )

  expect_class(learner$train(task), "LearnerClassifAutoResNet")
})
