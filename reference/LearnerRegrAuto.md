# Regression AutoML Learner

The LearnerRegrAuto is an automated machine learning (AutoML) system for
regression tasks. It combines preprocessing, a switch between multiple
learners, and hyperparameter tuning to find the best model for the given
task.

## Value

Object of class
[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) and
`LearnerRegrAuto`.

## Debugging

Set `options(bbotk.debug = TRUE)` to run the tuning in the main session.
Set `encapsulate_learner = FALSE` to remove encapsulation of the
learner. Set `encapsulate_mbo = FALSE` to catch no errors in mbo.

## Parameters

- learner_timeout:

  (`integer(1)`)  
  Timeout for training and predicting with a single learner.

- n_threads:

  (`integer(1)`)  
  Number of threads used for training a single learner.

- n_cpu:

  (named [`integer()`](https://rdrr.io/r/base/integer.html))  
  Number of CPUs a single training of a learner uses, named by learner
  id, e.g. `c(xgboost = 1)`. Overrides the default of the learner. Must
  be at least 1 because the worker always runs on the CPU. Currently
  informational only; the number of threads is controlled by
  `n_threads`.

- n_gpu:

  (named [`integer()`](https://rdrr.io/r/base/integer.html))  
  Number of GPUs a single training of a learner uses, named by learner
  id, e.g. `c(xgboost = 1)`. Overrides the default of the learner. Can
  only be 0 or 1 for now. The torch learners, TabPFN, TabFM, and fastai
  default to 1; all other learners default to 0. Only effective when
  `"cuda"` is part of `devices`; otherwise every learner is trained on
  the CPU. When the requirements are mixed and the daemons of the
  [mirai](https://CRAN.R-project.org/package=mirai) compute profiles
  `"mlr3automl_cpu"` and `"mlr3automl_gpu"` are set up with
  [`rush::rush_plan()`](https://rush.mlr-org.com/reference/rush_plan.html),
  the search space is partitioned into a cpu and a gpu subspace which
  are tuned with
  [mlr3mbo::TunerADBOSubspaces](https://mlr3mbo.mlr-org.com/reference/mlr_tuners_adbo_subspaces.html).
  The workers of a profile only ever propose and evaluate points of the
  subspace of that profile, so the number of workers per subspace is the
  number of workers of its profile. Otherwise the cpu and gpu learners
  are tuned in a single search space with
  [mlr3mbo::TunerAsyncMbo](https://mlr3mbo.mlr-org.com/reference/mlr_tuners_async_mbo.html).

      mirai::daemons(7, .compute = "mlr3automl_cpu")
      mirai::daemons(1, .compute = "mlr3automl_gpu")
      rush::rush_plan(profiles = c(mlr3automl_cpu = 7, mlr3automl_gpu = 1))

- memory_limit:

  (`integer(1)`)  
  Memory limit for training a single learner in MB. The limit is shared
  across the parallel workers, i.e. divided by the number of workers.

- devices:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Devices to use for model training. Possible values are `"cpu"` and
  `"cuda"`. If `"cuda"`, learners with a `n_gpu` requirement of 1 are
  trained on a GPU, while the remaining learners stay on the CPU.

- large_data_size:

  (`integer(1)`)  
  Threshold for the data set size (number of rows times number of
  columns) above which large-data rules apply. Beyond this threshold the
  number of parallel workers is reduced to a quarter, rounded up, and
  each remaining worker is given proportionally more threads and memory.
  When the workers are distributed over
  [mirai](https://CRAN.R-project.org/package=mirai) compute profiles,
  the number of workers of every profile is reduced, but every profile
  keeps at least one worker. The `"mlr3automl_gpu"` profile is exempt
  because its number of workers is fixed by the number of GPUs. It keeps
  its workers, threads, and memory limit so that the gpu learners do not
  claim the CPU cores and the memory that are freed on the cpu profiles.

- small_data_size:

  (`integer(1)`)  
  Threshold value for the data set size (rows) from which special rules
  apply.

- small_data_resampling:

  ([mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html))  
  Resampling strategy to use for model training on small data sets.

- initial_design_default:

  (`logical(1)`)  
  Whether to use the default design of the learner.

- initial_design_set:

  (`integer(1)`)  
  Number of points to use for the initial design set.

- initial_design_size:

  (`integer(1)`)  
  Size of the random, sobol or lhs initial design.

- initial_design_type:

  (`character(1)`)  
  Type of the initial design used for mbo. Possible values are `"lhs"`,
  `"sobol"`, `"random"`. `"lhs"` uses a Latin Hypercube Sampling design.
  `"sobol"` uses a Sobol sequence design. `"random"` uses a random
  design.

- initial_design_fraction:

  (`numeric(1)`)  
  Fraction of the budget to use for the initial design. When the search
  space is partitioned into a cpu and a gpu subspace, the remaining
  points of both designs are dropped, because every compute profile has
  its own queue.

- resampling:

  ([mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html))  
  Resampling strategy used for tuning.

- terminator:

  ([bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html))  
  Terminator criterion for tuning.

- measure:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  Measure used for tuning.

- callbacks:

  ([mlr3tuning::CallbackAsyncTuning](https://mlr3tuning.mlr-org.com/reference/CallbackAsyncTuning.html))  
  Callbacks used for tuning.

- store_benchmark_result:

  (`logical(1)`)  
  Whether to store the benchmark result.

- store_models:

  (`logical(1)`)  
  Whether to store the models.

- encapsulate_learner:

  (`logical(1)`)  
  Whether to encapsulate the learner. Change to `FALSE` to debug.

- encapsulate_mbo:

  (`logical(1)`)  
  Whether to encapsulate the tuning. Change to `FALSE` to debug.

- check_learners:

  (`logical(1)`)  
  Whether to check if the learners are compatible with the task. Change
  to `FALSE` to debug.

## Python learners

Python learners like `TabPFN`, `TabFM`, and `fastai` run via
`reticulate` and therefore need a Python installation with their
required packages. There are two ways to provide it:

1.  Do nothing and let
    [`reticulate::py_require()`](https://rstudio.github.io/reticulate/reference/py_require.html)
    install the required packages into an ephemeral virtual environment
    automatically.

2.  Point the `RETICULATE_PYTHON` environment variable to a Python
    installation that has the required packages installed.

We recommend option 2 when running on many workers, as it avoids the
overhead of downloading and installing the packages on each worker. Use
[`install_python_learners()`](https://mlr3automl.mlr-org.com/reference/install_python_learners.md)
to create a conda environment with the required packages and set
`RETICULATE_PYTHON` to the returned Python binary.

The `TabPFN` learner additionally requires the `TABPFN_TOKEN`
environment variable to download the model weights.

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`LearnerAuto`](https://mlr3automl.mlr-org.com/reference/LearnerAuto.md)
-\> `LearnerRegrAuto`

## Methods

### Public methods

- [`LearnerRegrAuto$new()`](#method-LearnerRegrAuto-initialize)

- [`LearnerRegrAuto$clone()`](#method-LearnerRegrAuto-clone)

Inherited methods

- [`mlr3::Learner$base_learner()`](https://mlr3.mlr-org.com/reference/Learner.html#method-base_learner)
- [`mlr3::Learner$configure()`](https://mlr3.mlr-org.com/reference/Learner.html#method-configure)
- [`mlr3::Learner$format()`](https://mlr3.mlr-org.com/reference/Learner.html#method-format)
- [`mlr3::Learner$help()`](https://mlr3.mlr-org.com/reference/Learner.html#method-help)
- [`mlr3::Learner$predict()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict)
- [`mlr3::Learner$predict_newdata()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict_newdata)
- [`mlr3::Learner$print()`](https://mlr3.mlr-org.com/reference/Learner.html#method-print)
- [`mlr3::Learner$reset()`](https://mlr3.mlr-org.com/reference/Learner.html#method-reset)
- [`mlr3::Learner$selected_features()`](https://mlr3.mlr-org.com/reference/Learner.html#method-selected_features)
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/reference/Learner.html#method-train)
- [`LearnerAuto$encapsulate()`](https://mlr3automl.mlr-org.com/reference/LearnerAuto.html#method-encapsulate)

------------------------------------------------------------------------

### `LearnerRegrAuto$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerRegrAuto$new(id = "regr.auto", learner_ids, rush = NULL)

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `learner_ids`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Learner that should be used.

- `rush`:

  [rush::Rush](https://rush.mlr-org.com/reference/Rush.html)  
  Rush instance.

------------------------------------------------------------------------

### `LearnerRegrAuto$clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerRegrAuto$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
packages = c(
  "mlr3extralearners", "catboost", "ranger", "callr", "mlr3torch",
  "glmnet", "kknn", "MASS", "lightgbm", "e1071", "xgboost"
)
if (mlr3misc::require_namespaces(packages, quietly = TRUE)) {
  learner = lrn("regr.auto")
  learner
}
```
