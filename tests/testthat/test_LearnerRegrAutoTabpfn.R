test_that("LearnerRegrAutoTabpfn works", {
  skip_if(TRUE)
  skip_on_cran()
  skip_if_not_all_installed(unlist(map(mlr_auto$mget("tabpfn"), "packages")))
  skip_if_not_installed("rush")
  skip_if_no_redis()

  expect_true(callr::r(function() {
    Sys.setenv(RETICULATE_PYTHON = "managed")
    library(mlr3automl)
    library(testthat)
    library(checkmate)
    lapply(
      list.files(system.file("testthat", package = "rush"), pattern = "^helper.*\\.[rR]", full.names = TRUE),
      source
    )

    rush = start_rush()
    on.exit({
      rush$reset()
      mirai::daemons(0)
    })

    mirai::everywhere({
      Sys.setenv(RETICULATE_PYTHON = "managed")
    })

    task = tsk("mtcars")

    learner = lrn(
      "regr.auto_tabpfn",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr("regr.rmse"),
      terminator = trm("evals", n_evals = 4),
      initial_design_type = "lhs",
      initial_design_size = 2,
      encapsulate_learner = FALSE,
      encapsulate_mbo = FALSE,
      check_learners = TRUE,
      rush = rush
    )

    expect_class(learner$train(task), "LearnerRegrAutoTabPFN")
    TRUE
  }))
})
