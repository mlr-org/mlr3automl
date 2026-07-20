test_that("LearnerClassifAutoCatboost works", {
  result = test_classif_learner("catboost")

  archive = as.data.table(result$learner$instance$archive, unnest = "internal_tuned_values")
  archive_finished = archive[state == "finished"]
  expect_integer(archive_finished$internal_tuned_values_catboost.iterations, lower = 1L)
})
