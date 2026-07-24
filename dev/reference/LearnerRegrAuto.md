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

- memory_limit:

  (`integer(1)`)  
  Memory limit for training a single learner in MB. The limit is shared
  across the parallel workers, i.e. divided by the number of workers.

- devices:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Devices to use for model training. Possible values are `"cpu"` and
  `"cuda"`. If `"cuda"`, the learner will be trained on a GPU.

- large_data_size:

  (`integer(1)`)  
  Threshold for the data set size (number of rows times number of
  columns) above which large-data rules apply. Beyond this threshold the
  number of parallel workers is reduced and each remaining worker is
  given proportionally more threads and memory.

- small_data_size:

  (`integer(1)`)  
  Threshold value for the data set size from which special rules apply.

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
  Fraction of the budget to use for the initial design.

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

Python learners like `TabPFN` and `fastai` run via `reticulate` and
therefore need a Python installation with their required packages. There
are two ways to provide it:

1.  Do nothing and let
    [`reticulate::py_require()`](https://rstudio.github.io/reticulate/reference/py_require.html)
    install the required packages into an ephemeral virtual environment
    automatically.

2.  Point the `RETICULATE_PYTHON` environment variable to a Python
    installation that has the required packages installed.

We recommend option 2 when running on many workers, as it avoids the
overhead of downloading and installing the packages on each worker. Use
[`install_python_learners()`](https://mlr3automl.mlr-org.com/dev/reference/install_python_learners.md)
to create a conda environment with the required packages and set
`RETICULATE_PYTHON` to the returned Python binary.

The `TabPFN` learner additionally requires the `TABPFN_TOKEN`
environment variable to download the model weights.

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`LearnerAuto`](https://mlr3automl.mlr-org.com/dev/reference/LearnerAuto.md)
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
- [`LearnerAuto$encapsulate()`](https://mlr3automl.mlr-org.com/dev/reference/LearnerAuto.html#method-encapsulate)

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
learner = lrn("regr.auto")
learner
#> 
#> ── <LearnerRegrAuto> (regr.auto) ───────────────────────────────────────────────
#> • Model: -
#> • Parameters: check_learners=TRUE, devices=cpu, encapsulate_learner=TRUE,
#> encapsulate_mbo=TRUE, initial_design_default=FALSE,
#> initial_design_fraction=0.25, initial_design_set=0, initial_design_size=256,
#> initial_design_type=sobol, large_data_size=1000000, learner_timeout=900,
#> memory_limit=32000, n_threads=1, resampling=<ResamplingHoldout>,
#> small_data_resampling=<ResamplingCV>, small_data_size=5000,
#> store_benchmark_result=FALSE, store_models=FALSE,
#> terminator=<TerminatorRunTime>
#> • Packages: mlr3, mlr3tuning, mlr3pipelines, mlr3learners, mlr3extralearners,
#> catboost, ranger, callr, mlr3torch, glmnet, kknn, MASS, lightgbm, e1071, and
#> xgboost
#> • Predict Types: [response]
#> • Feature Types: logical, integer, numeric, character, factor, and ordered
#> • Encapsulation: none (fallback: -)
#> • Properties: missings and weights
#> • Other settings: use_weights = 'use', predict_raw = 'FALSE'
```
