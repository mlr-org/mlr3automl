# mlr3automl (development version)

* feat: The auto learners gained the `n_cpu` and `n_gpu` parameters that override the per-learner resource requirements. When the requirements are mixed and `"cuda"` is part of `devices`, the search space is partitioned into a cpu and a gpu subspace and tuned with `mlr3mbo::TunerADBOSubspaces`, pinning one worker to the gpu subspace.
* BREAKING CHANGE: With `devices = c("cpu", "cuda")`, the boosting learners (xgboost, lightgbm, and catboost) now train on the CPU by default because their default `n_gpu` requirement is 0. Set e.g. `n_gpu = c(xgboost = 1)` to train them on the GPU again.

* Initial CRAN release.
