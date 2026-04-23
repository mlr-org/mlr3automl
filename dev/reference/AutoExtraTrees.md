# Extra Trees Auto

Extra Trees auto.

## Super class

[`mlr3automl::Auto`](https://mlr3automl.mlr-org.com/dev/reference/Auto.md)
-\> `AutoExtraTrees`

## Methods

### Public methods

- [`AutoExtraTrees$new()`](#method-AutoExtraTrees-new)

- [`AutoExtraTrees$graph()`](#method-AutoExtraTrees-graph)

- [`AutoExtraTrees$estimate_memory()`](#method-AutoExtraTrees-estimate_memory)

- [`AutoExtraTrees$clone()`](#method-AutoExtraTrees-clone)

Inherited methods

- [`mlr3automl::Auto$check()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-check)
- [`mlr3automl::Auto$design_default()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_default)
- [`mlr3automl::Auto$design_set()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_set)
- [`mlr3automl::Auto$early_stopping_rounds()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-early_stopping_rounds)
- [`mlr3automl::Auto$finalize_model()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-finalize_model)
- [`mlr3automl::Auto$search_space()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-search_space)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoExtraTrees$new(id = "extra_trees")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### Method `graph()`

Create the graph for the auto.

#### Usage

    AutoExtraTrees$graph(task, measure, n_threads, timeout, devices)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)).

- `n_threads`:

  (`numeric(1)`).

- `timeout`:

  (`numeric(1)`).

- `devices`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Devices to use. Allowed values are `"cpu"` and `"cuda"`. Default is
  "cpu".

------------------------------------------------------------------------

### Method `estimate_memory()`

Estimate the memory for the auto.

#### Usage

    AutoExtraTrees$estimate_memory(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoExtraTrees$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
