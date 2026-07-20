test_that("LearnerRegrAutoLightGBM works", {
  result = test_regr_learner("lightgbm")

  archive = as.data.table(result$learner$instance$archive, unnest = "internal_tuned_values")
  archive_finished = archive[state == "finished"]
  expect_integer(archive_finished$internal_tuned_values_lightgbm.num_iterations, lower = 1L)
})
