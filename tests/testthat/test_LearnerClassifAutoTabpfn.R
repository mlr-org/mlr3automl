test_that("LearnerClassifAutoTabpfn works", {
  skip_if(TRUE)
  skip_on_cran()
  skip_if_not_installed(unlist(map(mlr_auto$mget("tabpfn"), "packages")))
  skip_if_not_installed("rush")
  flush_redis()

  expect_true(callr::r(function() {
    Sys.setenv(RETICULATE_PYTHON = "managed")
    library(mlr3automl)
    library(testthat)
    library(checkmate)

    rush_plan(n_workers = 2, worker_type = "remote")
    mirai::daemons(2)

    mirai::everywhere({
      Sys.setenv(RETICULATE_PYTHON = "managed")
    })

    task = tsk("penguins")
    task$filter(c(1, 153, 277))

    learner = lrn("classif.auto_tabpfn",
      small_data_size = 1,
      resampling = rsmp("holdout"),
      measure = msr("classif.ce"),
      terminator = trm("evals", n_evals = 4),
      initial_design_type = "lhs",
      initial_design_size = 2,
      encapsulate_learner = FALSE,
      encapsulate_mbo = FALSE,
      check_learners = TRUE)

    expect_class(learner$train(task), "LearnerClassifAutoTabPFN")
  }))
})


