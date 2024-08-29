rush_plan(n_workers = 2)
skip_if_not_installed("ranger")

task = tsk("penguins")
learner = lrn("classif.auto",
  learner_ids = "ranger",
  small_data_size = 1,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  terminator = trm("evals", n_evals = 6)
)
learner$train(task)

test_that("cost over time works", {
  plot = cost_over_time(learner$instance)
  expect_class(plot, c("gg", "ggplot"))
})
