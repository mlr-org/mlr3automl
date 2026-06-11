# Svm Auto

Svm auto.

## Super class

[`Auto`](https://mlr3automl.mlr-org.com/dev/reference/Auto.md) -\>
`AutoSvm`

## Methods

### Public methods

- [`AutoSvm$new()`](#method-AutoSvm-initialize)

- [`AutoSvm$graph()`](#method-AutoSvm-graph)

- [`AutoSvm$design_default()`](#method-AutoSvm-design_default)

- [`AutoSvm$clone()`](#method-AutoSvm-clone)

Inherited methods

- [`Auto$check()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-check)
- [`Auto$design_set()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_set)
- [`Auto$early_stopping_rounds()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-early_stopping_rounds)
- [`Auto$estimate_memory()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-estimate_memory)
- [`Auto$finalize_model()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-finalize_model)
- [`Auto$search_space()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-search_space)

------------------------------------------------------------------------

### `AutoSvm$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoSvm$new(id = "svm")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### `AutoSvm$graph()`

Create the graph for the auto.

#### Usage

    AutoSvm$graph(task, measure, n_threads, timeout, devices)

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

### `AutoSvm$design_default()`

Default hyperparameters for the learner.

#### Usage

    AutoSvm$design_default(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoSvm$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoSvm$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
