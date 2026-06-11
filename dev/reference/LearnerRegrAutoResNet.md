# Regression ResNet Auto Learner

Regression auto learner.

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`LearnerAuto`](https://mlr3automl.mlr-org.com/dev/reference/LearnerAuto.md)
-\>
[`LearnerRegrAuto`](https://mlr3automl.mlr-org.com/dev/reference/LearnerRegrAuto.md)
-\> `LearnerRegrAutoResNet`

## Methods

### Public methods

- [`LearnerRegrAutoResNet$new()`](#method-LearnerRegrAutoResNet-initialize)

- [`LearnerRegrAutoResNet$clone()`](#method-LearnerRegrAutoResNet-clone)

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

### `LearnerRegrAutoResNet$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerRegrAutoResNet$new(id = "regr.auto_resnet", rush = NULL)

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `rush`:

  [rush::Rush](https://rush.mlr-org.com/reference/Rush.html)  
  Rush instance.

------------------------------------------------------------------------

### `LearnerRegrAutoResNet$clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerRegrAutoResNet$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
