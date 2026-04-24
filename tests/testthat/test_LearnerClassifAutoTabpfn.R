test_that("LearnerClassifAutoTabpfn works", {
  skip_if(TRUE)
  skip_on_cran()
  skip_if_auto_not_installed("tabpfn")
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

    task = tsk("penguins")
    task$filter(c(1, 153, 277))

    learner = lrn(
      "classif.auto_tabpfn",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr("classif.ce"),
      terminator = trm("evals", n_evals = 4),
      initial_design_type = "lhs",
      initial_design_size = 2,
      encapsulate_learner = FALSE,
      encapsulate_mbo = FALSE,
      check_learners = TRUE,
      rush = rush
    )

    expect_class(learner$train(task), "LearnerClassifAutoTabPFN")
  }))
})
