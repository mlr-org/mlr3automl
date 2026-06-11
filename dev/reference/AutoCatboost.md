# Catboost Auto

Catboost auto.

## Super class

[`Auto`](https://mlr3automl.mlr-org.com/dev/reference/Auto.md) -\>
`AutoCatboost`

## Methods

### Public methods

- [`AutoCatboost$new()`](#method-AutoCatboost-initialize)

- [`AutoCatboost$graph()`](#method-AutoCatboost-graph)

- [`AutoCatboost$estimate_memory()`](#method-AutoCatboost-estimate_memory)

- [`AutoCatboost$internal_measure()`](#method-AutoCatboost-internal_measure)

- [`AutoCatboost$clone()`](#method-AutoCatboost-clone)

Inherited methods

- [`Auto$check()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-check)
- [`Auto$design_default()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_default)
- [`Auto$design_set()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_set)
- [`Auto$early_stopping_rounds()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-early_stopping_rounds)
- [`Auto$finalize_model()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-finalize_model)
- [`Auto$search_space()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-search_space)

------------------------------------------------------------------------

### `AutoCatboost$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoCatboost$new(id = "catboost")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### `AutoCatboost$graph()`

Create the graph for the auto.

#### Usage

    AutoCatboost$graph(task, measure, n_threads, timeout, devices)

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

### `AutoCatboost$estimate_memory()`

Estimate the memory for the auto.

#### Usage

    AutoCatboost$estimate_memory(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoCatboost$internal_measure()`

Get the internal measure for the auto.

#### Usage

    AutoCatboost$internal_measure(measure, task)

#### Arguments

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)).

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoCatboost$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoCatboost$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
