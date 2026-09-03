# TabFM Learner Isolated

A subclass of
[mlr3extralearners::LearnerClassifTabFM](https://mlr3extralearners.mlr-org.com/reference/mlr_learners_classif.tabfm.html)
that isolates the Python environment in a callr session.

## Value

Object of class
[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) and
`LearnerClassifTabFMIsolated`.

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`mlr3::LearnerClassif`](https://mlr3.mlr-org.com/reference/LearnerClassif.html)
-\>
[`mlr3extralearners::LearnerClassifTabFM`](https://mlr3extralearners.mlr-org.com/reference/mlr_learners_classif.tabfm.html)
-\> `LearnerClassifTabFMIsolated`

## Public fields

- `isolate_python`:

  (`logical(1)`)  
  Whether to run `.train()` and `.predict()` in a fresh callr session.
  Set by
  [AutoTabFM](https://mlr3automl.mlr-org.com/reference/AutoTabFM.md)`$graph()`;
  only `FALSE` when the run's learners never load mlr3torch.

## Methods

### Public methods

- [`LearnerClassifTabFMIsolated$new()`](#method-LearnerClassifTabFMIsolated-initialize)

- [`LearnerClassifTabFMIsolated$clone()`](#method-LearnerClassifTabFMIsolated-clone)

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
- [`mlr3::LearnerClassif$predict_newdata_fast()`](https://mlr3.mlr-org.com/reference/LearnerClassif.html#method-predict_newdata_fast)
- [`mlr3extralearners::LearnerClassifTabFM$marshal()`](https://mlr3extralearners.mlr-org.com/reference/mlr_learners_classif.tabfm.html#method-marshal)
- [`mlr3extralearners::LearnerClassifTabFM$unmarshal()`](https://mlr3extralearners.mlr-org.com/reference/mlr_learners_classif.tabfm.html#method-unmarshal)

------------------------------------------------------------------------

### `LearnerClassifTabFMIsolated$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerClassifTabFMIsolated$new()

------------------------------------------------------------------------

### `LearnerClassifTabFMIsolated$clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerClassifTabFMIsolated$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
