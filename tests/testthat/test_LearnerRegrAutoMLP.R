test_that("LearnerRegrAutoMLP works", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed(), "torch backend (LibTorch) not installed")

  result = test_regr_learner("mlp", check_learners = FALSE)

  archive = as.data.table(result$learner$instance$archive, unnest = "internal_tuned_values")
  archive_finished = archive[state == "finished"]
  expect_integer(archive_finished$internal_tuned_values_mlp.epochs, lower = 1L)
})
