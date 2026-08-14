test_that("LearnerRegrAutoTabFM works", {
  # tabfm is registered for cuda only, so the end-to-end test runs a variant that also allows the cpu
  mlr_auto$add("tabfm", function() AutoTabFM$new(devices = c("cpu", "cuda")))
  on.exit(mlr_auto$add("tabfm", function() AutoTabFM$new()), add = TRUE)

  # every evaluation runs the backbone once per estimator in its own callr session,
  # so the budget is kept at the minimum that still exercises the initial design and one mbo proposal
  test_regr_learner("tabfm", initial_design_size = 1, n_evals = 2)
})
