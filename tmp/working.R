devtools::load_all()
set.seed(1453)

rush_plan(n_workers = 1)
task = tsk("penguins")
learner = lrn("classif.auto",
  learner_ids = c("glmnet", "xgboost"),
  small_data_size = 1,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  terminator = trm("evals", n_evals = 20)
)

learner$train(task)

res <- config_footprint(learner$instance)
