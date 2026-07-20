test_that("LearnerClassifAutoResNet works", {
  skip_if_not_installed("torch")
  skip_if(!torch::torch_is_installed(), "torch backend (LibTorch) not installed")

  task = tsk("penguins")
  task$filter(c(1:10, 153:162, 277:286))

  result = test_classif_learner("resnet", task = task, check_learners = FALSE)

  archive = as.data.table(result$learner$instance$archive, unnest = "internal_tuned_values")
  archive_finished = archive[state == "finished"]
  expect_integer(archive_finished$internal_tuned_values_resnet.epochs, lower = 1L)
})
