# Regression XGBoost Auto Learner

Regression auto learner.

## Value

Object of class
[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) and
`LearnerRegrAutoXgboost`.

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`LearnerAuto`](https://mlr3automl.mlr-org.com/dev/reference/LearnerAuto.md)
-\>
[`LearnerRegrAuto`](https://mlr3automl.mlr-org.com/dev/reference/LearnerRegrAuto.md)
-\> `LearnerRegrAutoXgboost`

## Methods

### Public methods

- [`LearnerRegrAutoXgboost$new()`](#method-LearnerRegrAutoXgboost-initialize)

- [`LearnerRegrAutoXgboost$clone()`](#method-LearnerRegrAutoXgboost-clone)

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

### `LearnerRegrAutoXgboost$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerRegrAutoXgboost$new(id = "regr.auto_xgboost", rush = NULL)

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `rush`:

  [rush::Rush](https://rush.mlr-org.com/reference/Rush.html)  
  Rush instance.

------------------------------------------------------------------------

### `LearnerRegrAutoXgboost$clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerRegrAutoXgboost$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
learner = lrn("regr.auto_xgboost")
learner
#> 
#> ── <LearnerRegrAutoXgboost> (regr.auto_xgboost) ────────────────────────────────
#> • Model: -
#> • Parameters: check_learners=TRUE, devices=cpu, encapsulate_learner=TRUE,
#> encapsulate_mbo=TRUE, initial_design_default=FALSE,
#> initial_design_fraction=0.25, initial_design_set=0, initial_design_size=256,
#> initial_design_type=sobol, large_data_size=1000000, learner_timeout=900,
#> memory_limit=32000, n_threads=1, resampling=<ResamplingHoldout>,
#> small_data_resampling=<ResamplingCV>, small_data_size=5000,
#> store_benchmark_result=FALSE, store_models=FALSE,
#> terminator=<TerminatorRunTime>
#> • Packages: mlr3, mlr3tuning, mlr3pipelines, mlr3learners, and xgboost
#> • Predict Types: [response]
#> • Feature Types: logical, integer, numeric, character, factor, and ordered
#> • Encapsulation: none (fallback: -)
#> • Properties: missings and weights
#> • Other settings: use_weights = 'use', predict_raw = 'FALSE'
```
