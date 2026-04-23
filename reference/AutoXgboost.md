# Xgboost Auto

Xgboost auto.

## Super class

[`mlr3automl::Auto`](https://mlr3automl.mlr-org.com/reference/Auto.md)
-\> `AutoXgboost`

## Methods

### Public methods

- [`AutoXgboost$new()`](#method-AutoXgboost-new)

- [`AutoXgboost$graph()`](#method-AutoXgboost-graph)

- [`AutoXgboost$finalize_model()`](#method-AutoXgboost-finalize_model)

- [`AutoXgboost$estimate_memory()`](#method-AutoXgboost-estimate_memory)

- [`AutoXgboost$internal_measure()`](#method-AutoXgboost-internal_measure)

- [`AutoXgboost$clone()`](#method-AutoXgboost-clone)

Inherited methods

- [`mlr3automl::Auto$check()`](https://mlr3automl.mlr-org.com/reference/Auto.html#method-check)
- [`mlr3automl::Auto$design_default()`](https://mlr3automl.mlr-org.com/reference/Auto.html#method-design_default)
- [`mlr3automl::Auto$design_set()`](https://mlr3automl.mlr-org.com/reference/Auto.html#method-design_set)
- [`mlr3automl::Auto$early_stopping_rounds()`](https://mlr3automl.mlr-org.com/reference/Auto.html#method-early_stopping_rounds)
- [`mlr3automl::Auto$search_space()`](https://mlr3automl.mlr-org.com/reference/Auto.html#method-search_space)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoXgboost$new(id = "xgboost")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### Method `graph()`

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

### Method `finalize_model()`

Prepare the graph learner for the final model fit.

#### Usage

    AutoXgboost$finalize_model(graph_learner)

------------------------------------------------------------------------

### Method `estimate_memory()`

Estimate the memory for the auto.

#### Usage

    AutoXgboost$estimate_memory(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### Method `internal_measure()`

Get the internal measure for the auto.

#### Usage

    AutoXgboost$internal_measure(measure, task)

#### Arguments

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)).

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoXgboost$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
