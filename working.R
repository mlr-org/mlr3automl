rush_plan(n_workers = 3)
task = tsk("spam")
learner = lrn("classif.auto",
  learner_ids = c("glmnet", "svm"),
  small_data_size = 1,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  terminator = trm("evals", n_evals = 20)
)
learner$train(task)

# instance = learner$instance


# cost over time plot
cost_over_time = function(instance) {
  archive_table = copy(learner$instance$archive$data)
  set(archive_table, j = "config_id", value = seq_len(nrow(archive_table)))

  # there should be only a single objective, e.g. `classif.ce`
  cost = instance$objective$codomain$data$id[[1]]
  
  ggplot2::ggplot(data = archive_table) +
    ggplot2::geom_point(ggplot2::aes(x = config_id, y = .data[[cost]]))
}

cost_over_time(instance) +
  ggplot2::theme_bw()
