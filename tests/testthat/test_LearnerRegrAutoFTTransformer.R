test_that("LearnerRegrAutoFTTransformer works", {
  skip_if(!torch::torch_is_installed(), "torch backend (LibTorch) not installed")

  result = test_regr_learner("ft_transformer", check_learners = FALSE)

  archive = as.data.table(result$learner$instance$archive, unnest = "internal_tuned_values")
  archive_finished = archive[state == "finished"]
  expect_integer(archive_finished$internal_tuned_values_ft_transformer.epochs, lower = 1L)
})
