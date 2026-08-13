# Changelog

## mlr3automl (development version)

- fix: The default configuration of the tabpfn learner in the initial
  design now matches TabPFN-3. The number of estimators changed from 4
  to 8 and the softmax temperature from 1.0 to 0.9, which were the
  defaults of TabPFN-2.

- fix: The tabpfn learner now sets `auto_scale_n_estimators = FALSE`.
  TabPFN-3 otherwise raises the number of estimators on its own when the
  task has more features than a single ensemble member sees, which
  overrides the tuned value. This requires `mlr3extralearners`
  1.6.0.9000 or later, which is now the minimum version.

- fix: Tuning on subspaces no longer fails when a learner has internally
  tuned parameters, e.g. `xgboost.nrounds` or `ft_transformer.epochs`.
  The subspaces are now derived from the search space of the tuning
  instance, which no longer holds the internally tuned parameters.

- fix: The torch learners (mlp, resnet, and ft_transformer) now train on
  the CPU when `devices` does not include `"cuda"`. Previously they
  passed `device = "auto"` to `mlr3torch`, which selects the GPU
  whenever one is available, so they ignored `devices = "cpu"` and the
  resource accounting of the workers.

- fix: The torch learners (mlp, resnet, and ft_transformer) now train
  with a batch size of 256 instead of 32 on the GPU. A batch of 32 rows
  leaves the GPU mostly idle because kernel launches and host to device
  transfers dominate the step time.

- fix: The torch learners (mlp, resnet, and ft_transformer) are no
  longer removed from the search space by `memory_limit` when they train
  on the GPU. Their memory estimates are fitted on host memory
  measurements, which do not describe the memory the model allocates on
  the GPU.

- fix: The gpu compute profile is now exempt from the large data set
  rules. Its number of workers is fixed by the number of GPUs, so
  reducing it does not free any resources, and its worker previously
  received the CPU cores and the memory that are freed on the cpu
  profiles.

- fix: The number of workers of a compute profile is now reduced to a
  quarter rounded up instead of rounded down, so a profile no longer
  loses more than the intended factor, e.g. 7 workers are reduced to 2
  instead of 1.

- feat: The auto learners gained the `n_cpu` and `n_gpu` parameters that
  override the per-learner resource requirements. When the requirements
  are mixed, `"cuda"` is part of `devices`, and the workers are
  distributed over the `mirai` compute profiles `"mlr3automl_cpu"` and
  `"mlr3automl_gpu"` with
  `rush::rush_plan(profiles = c(mlr3automl_cpu = 7, mlr3automl_gpu = 1))`,
  the search space is partitioned into a cpu and a gpu subspace and
  tuned with
  [`mlr3mbo::TunerADBOSubspaces`](https://mlr3mbo.mlr-org.com/reference/mlr_tuners_adbo_subspaces.html).
  The workers of a profile only evaluate points of the subspace of that
  profile.

- BREAKING CHANGE: With `devices = c("cpu", "cuda")`, the boosting
  learners (xgboost, lightgbm, and catboost) now train on the CPU by
  default because their default `n_gpu` requirement is 0. Set
  e.g. `n_gpu = c(xgboost = 1)` to train them on the GPU again.

- Initial CRAN release.
