test_that("LearnerClassifAutoXgboost works", {
  result = test_classif_learner("xgboost")

  archive = as.data.table(result$learner$instance$archive, unnest = "internal_tuned_values")
  archive_finished = archive[state == "finished"]
  expect_integer(archive_finished$internal_tuned_values_xgboost.nrounds, lower = 1L)
})
