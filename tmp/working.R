devtools::load_all()
set.seed(1453)

flush_redis()
rush_plan(n_workers = 1)
task = tsk("penguins")
learner = lrn("classif.auto",
  learner_ids = c("kknn", "glmnet"),
  small_data_size = 1,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  terminator = trm("evals", n_evals = 20)
)

learner$train(task)

library(reticulate)
use_condaenv("DeepCAVE")
source_python("tmp/to_source.py")

instance = learner$instance
config_footprint(instance)

View(distances)
View(fp_distances)

archive$data[c(1,3), archive$cols_x, with = FALSE]
fp_distances[1, 3]
distances[1, 3]

encode_configs(archive$data[c(9,2), archive$cols_x, with = FALSE], configspace)
View(encode_configs(border_configs, configspace))
