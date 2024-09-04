rush_plan(n_workers = 3)
task = tsk("spam")
learner = lrn("classif.auto",
  learner_ids = c("ranger", "glmnet", "svm"),
  small_data_size = 1,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  terminator = trm("evals", n_evals = 20)
)
learner$train(task)

archive = learner$instance$archive$clone(deep = TRUE)

param_ids = archive$cols_x[startsWith(archive$cols_x, "glmnet")]
branch = "glmnet"

surrogate = default_surrogate(learner$instance)
surrogate$archive = archive
surrogate$update()

predictor = iml::Predictor$new(
  model = surrogate,
  data = as.data.table(archive)[branch.selection == branch, param_ids, with = FALSE],
  predict.function = function(model, newdata) {
    model$predict(setDT(newdata)[, param_ids, with = FALSE])$mean
  }
)

effects = iml::FeatureEffect$new(
  predictor,
  param_ids,
  method = "pdp"
)
effects$plot() +
  ggplot2::scale_fill_viridis_c()
