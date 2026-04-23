# Regression XGBoost Auto Learner

Regression auto learner.

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`mlr3automl::LearnerAuto`](https://mlr3automl.mlr-org.com/reference/LearnerAuto.md)
-\>
[`mlr3automl::LearnerRegrAuto`](https://mlr3automl.mlr-org.com/reference/LearnerRegrAuto.md)
-\> `LearnerRegrAutoXgboost`

## Methods

### Public methods

- [`LearnerRegrAutoXgboost$new()`](#method-LearnerRegrAutoXgboost-new)

- [`LearnerRegrAutoXgboost$clone()`](#method-LearnerRegrAutoXgboost-clone)

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

------------------------------------------------------------------------

### Method `new()`

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

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerRegrAutoXgboost$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
