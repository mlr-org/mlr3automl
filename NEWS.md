# mlr3automl (development version)

* feat: The auto learners gained the `n_cpu` and `n_gpu` parameters that override the per-learner resource requirements. When the requirements are mixed, `"cuda"` is part of `devices`, and the workers are distributed over a cpu and a gpu `mirai` compute profile with `rush::rush_plan(profiles = c(cpu = 7, gpu = 1))`, the search space is partitioned into a cpu and a gpu subspace and tuned with `mlr3mbo::TunerADBOSubspaces`. The workers of a profile only evaluate points of the subspace of that profile.
* feat: The auto learners gained the `subspace_profiles` parameter that maps the cpu and the gpu subspace to differently named `mirai` compute profiles, e.g. `c(cpu = "cores", gpu = "cuda")`.
* BREAKING CHANGE: With `devices = c("cpu", "cuda")`, the boosting learners (xgboost, lightgbm, and catboost) now train on the CPU by default because their default `n_gpu` requirement is 0. Set e.g. `n_gpu = c(xgboost = 1)` to train them on the GPU again.

* Initial CRAN release.
