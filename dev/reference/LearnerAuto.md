# Base AutoML Learner

Abstract base class for AutoML learners. Contains the shared logic for
[LearnerClassifAuto](https://mlr3automl.mlr-org.com/dev/reference/LearnerClassifAuto.md)
and
[LearnerRegrAuto](https://mlr3automl.mlr-org.com/dev/reference/LearnerRegrAuto.md).

The following learners are supported:

|                |                   |
|----------------|-------------------|
| Algorithm      | Package           |
| catboost       | mlr3extralearners |
| extra_trees    | mlr3learners      |
| fastai         | mlr3extralearners |
| ft_transformer | mlr3torch         |
| glmnet         | mlr3learners      |
| kknn           | mlr3learners      |
| lda            | mlr3learners      |
| lightgbm       | mlr3extralearners |
| mlp            | mlr3torch         |
| ranger         | mlr3learners      |
| resnet         | mlr3torch         |
| svm            | mlr3learners      |
| tabpfn         | mlr3extralearners |
| xgboost        | mlr3learners      |

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

## Super class

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
`LearnerAuto`

## Public fields

- `instance`:

  ([mlr3tuning::TuningInstanceAsyncSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceAsyncSingleCrit.html)).

- `rush`:

  ([rush::Rush](https://rush.mlr-org.com/reference/Rush.html))  
  Rush instance for parallel tuning.

## Methods

### Public methods

- [`LearnerAuto$new()`](#method-LearnerAuto-initialize)

- [`LearnerAuto$encapsulate()`](#method-LearnerAuto-encapsulate)

- [`LearnerAuto$clone()`](#method-LearnerAuto-clone)

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

------------------------------------------------------------------------

### `LearnerAuto$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerAuto$new(
      id,
      learner_ids,
      task_type,
      predict_types,
      properties,
      rush = NULL
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `learner_ids`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Learner that should be used.

- `task_type`:

  (`character(1)`)  
  The task type, either `"classif"` or `"regr"`.

- `predict_types`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Supported predict types.

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Learner properties.

- `rush`:

  [rush::Rush](https://rush.mlr-org.com/reference/Rush.html)  
  Rush instance.

------------------------------------------------------------------------

### `LearnerAuto$encapsulate()`

Redirects encapsulation to the final model fit.

The AutoML learner itself always trains in the main session, because the
rush-based parallel tuning cannot run inside an encapsulated session.
The encapsulation method and fallback learner are instead applied to the
final model fit that follows the tuning phase. If the final model fit
fails, the fallback learner is trained instead. Without encapsulation, a
failed final model fit raises an error. The tuning phase is guarded by
the `encapsulate_learner` and `encapsulate_mbo` parameters instead.

#### Usage

    LearnerAuto$encapsulate(method, fallback = NULL, when = NULL)

#### Arguments

- `method`:

  (`character(1)`)  
  One of `"none"`, `"try"`, `"evaluate"`, `"callr"`, or `"mirai"`.

- `fallback`:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))  
  Learner to train when the final model fit fails.

- `when`:

  (`function()`)  
  Optional condition handler passed to the `$encapsulate()` method of
  the final model.

#### Returns

`self` (invisibly).

------------------------------------------------------------------------

### `LearnerAuto$clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerAuto$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
