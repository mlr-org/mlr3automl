# TabFM Regressor Learner Isolated

A subclass of
[mlr3extralearners::LearnerRegrTabFM](https://mlr3extralearners.mlr-org.com/reference/mlr_learners_regr.tabfm.html)
that isolates the Python environment in a callr session.

## Value

Object of class
[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) and
`LearnerRegrTabFMIsolated`.

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`mlr3::LearnerRegr`](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
-\>
[`mlr3extralearners::LearnerRegrTabFM`](https://mlr3extralearners.mlr-org.com/reference/mlr_learners_regr.tabfm.html)
-\> `LearnerRegrTabFMIsolated`

## Methods

### Public methods

- [`LearnerRegrTabFMIsolated$new()`](#method-LearnerRegrTabFMIsolated-initialize)

- [`LearnerRegrTabFMIsolated$clone()`](#method-LearnerRegrTabFMIsolated-clone)

Inherited methods

- [`mlr3::Learner$base_learner()`](https://mlr3.mlr-org.com/reference/Learner.html#method-base_learner)
- [`mlr3::Learner$configure()`](https://mlr3.mlr-org.com/reference/Learner.html#method-configure)
- [`mlr3::Learner$encapsulate()`](https://mlr3.mlr-org.com/reference/Learner.html#method-encapsulate)
- [`mlr3::Learner$format()`](https://mlr3.mlr-org.com/reference/Learner.html#method-format)
- [`mlr3::Learner$help()`](https://mlr3.mlr-org.com/reference/Learner.html#method-help)
- [`mlr3::Learner$predict()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict)
- [`mlr3::Learner$predict_newdata()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict_newdata)
- [`mlr3::Learner$print()`](https://mlr3.mlr-org.com/reference/Learner.html#method-print)
- [`mlr3::Learner$reset()`](https://mlr3.mlr-org.com/reference/Learner.html#method-reset)
- [`mlr3::Learner$selected_features()`](https://mlr3.mlr-org.com/reference/Learner.html#method-selected_features)
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/reference/Learner.html#method-train)
- [`mlr3::LearnerRegr$predict_newdata_fast()`](https://mlr3.mlr-org.com/reference/LearnerRegr.html#method-predict_newdata_fast)
- [`mlr3extralearners::LearnerRegrTabFM$marshal()`](https://mlr3extralearners.mlr-org.com/reference/mlr_learners_regr.tabfm.html#method-marshal)
- [`mlr3extralearners::LearnerRegrTabFM$unmarshal()`](https://mlr3extralearners.mlr-org.com/reference/mlr_learners_regr.tabfm.html#method-unmarshal)

------------------------------------------------------------------------

### `LearnerRegrTabFMIsolated$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerRegrTabFMIsolated$new()

------------------------------------------------------------------------

### `LearnerRegrTabFMIsolated$clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerRegrTabFMIsolated$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
