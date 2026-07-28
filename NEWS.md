# mlr3automl (development version)

* fix: Tuning on subspaces no longer fails when a learner has internally tuned parameters, e.g. `xgboost.nrounds` or `ft_transformer.epochs`. The subspaces are now derived from the search space of the tuning instance, which no longer holds the internally tuned parameters.
* fix: The torch learners (mlp, resnet, and ft_transformer) now train on the CPU when `devices` does not include `"cuda"`. Previously they passed `device = "auto"` to `mlr3torch`, which selects the GPU whenever one is available, so they ignored `devices = "cpu"` and the resource accounting of the workers.
* fix: The torch learners (mlp, resnet, and ft_transformer) now train with a batch size of 256 instead of 32 on the GPU. A batch of 32 rows leaves the GPU mostly idle because kernel launches and host to device transfers dominate the step time.
* fix: The torch learners (mlp, resnet, and ft_transformer) are no longer removed from the search space by `memory_limit` when they train on the GPU. Their memory estimates are fitted on host memory measurements, which do not describe the memory the model allocates on the GPU.
* feat: The auto learners gained the `n_cpu` and `n_gpu` parameters that override the per-learner resource requirements. When the requirements are mixed, `"cuda"` is part of `devices`, and the workers are distributed over a cpu and a gpu `mirai` compute profile with `rush::rush_plan(profiles = c(cpu = 7, gpu = 1))`, the search space is partitioned into a cpu and a gpu subspace and tuned with `mlr3mbo::TunerADBOSubspaces`. The workers of a profile only evaluate points of the subspace of that profile.
* feat: The auto learners gained the `subspace_profiles` parameter that maps the cpu and the gpu subspace to differently named `mirai` compute profiles, e.g. `c(cpu = "cores", gpu = "cuda")`.
* BREAKING CHANGE: With `devices = c("cpu", "cuda")`, the boosting learners (xgboost, lightgbm, and catboost) now train on the CPU by default because their default `n_gpu` requirement is 0. Set e.g. `n_gpu = c(xgboost = 1)` to train them on the GPU again.

* Initial CRAN release.
