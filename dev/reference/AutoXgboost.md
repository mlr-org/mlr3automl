# Xgboost Auto

Xgboost auto.

## Super class

[`Auto`](https://mlr3automl.mlr-org.com/dev/reference/Auto.md) -\>
`AutoXgboost`

## Methods

### Public methods

- [`AutoXgboost$new()`](#method-AutoXgboost-initialize)

- [`AutoXgboost$graph()`](#method-AutoXgboost-graph)

- [`AutoXgboost$finalize_model()`](#method-AutoXgboost-finalize_model)

- [`AutoXgboost$estimate_memory()`](#method-AutoXgboost-estimate_memory)

- [`AutoXgboost$internal_measure()`](#method-AutoXgboost-internal_measure)

- [`AutoXgboost$clone()`](#method-AutoXgboost-clone)

Inherited methods

- [`Auto$check()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-check)
- [`Auto$design_default()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_default)
- [`Auto$design_set()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_set)
- [`Auto$early_stopping_rounds()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-early_stopping_rounds)
- [`Auto$search_space()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-search_space)

------------------------------------------------------------------------

### `AutoXgboost$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoXgboost$new(id = "xgboost")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### `AutoXgboost$graph()`

Create the graph for the auto.

#### Usage

    AutoXgboost$graph(task, measure, n_threads, timeout, devices)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)).

- `n_threads`:

  (`integer(1)`).

- `timeout`:

  (`integer(1)`).

- `devices`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Devices to use. Allowed values are `"cpu"` and `"cuda"`. Default is
  "cpu".

------------------------------------------------------------------------

### `AutoXgboost$finalize_model()`

Prepare the graph learner for the final model fit.

#### Usage

    AutoXgboost$finalize_model(graph_learner)

#### Arguments

- `graph_learner`:

  ([mlr3pipelines::GraphLearner](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_graph.html)).

------------------------------------------------------------------------

### `AutoXgboost$estimate_memory()`

Estimate the memory for the auto.

#### Usage

    AutoXgboost$estimate_memory(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoXgboost$internal_measure()`

Get the internal measure for the auto.

#### Usage

    AutoXgboost$internal_measure(measure, task)

#### Arguments

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)).

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoXgboost$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoXgboost$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
