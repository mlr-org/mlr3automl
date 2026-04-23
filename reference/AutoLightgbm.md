# Lightgbm Auto

Lightgbm auto.

## Super class

[`mlr3automl::Auto`](https://mlr3automl.mlr-org.com/reference/Auto.md)
-\> `AutoLightgbm`

## Methods

### Public methods

- [`AutoLightgbm$new()`](#method-AutoLightgbm-new)

- [`AutoLightgbm$graph()`](#method-AutoLightgbm-graph)

- [`AutoLightgbm$finalize_model()`](#method-AutoLightgbm-finalize_model)

- [`AutoLightgbm$estimate_memory()`](#method-AutoLightgbm-estimate_memory)

- [`AutoLightgbm$internal_measure()`](#method-AutoLightgbm-internal_measure)

- [`AutoLightgbm$clone()`](#method-AutoLightgbm-clone)

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

    AutoLightgbm$new(id = "lightgbm")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### Method `graph()`

Create the graph for the auto.

#### Usage

    AutoLightgbm$graph(task, measure, n_threads, timeout, devices)

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

    AutoLightgbm$finalize_model(graph_learner)

------------------------------------------------------------------------

### Method `estimate_memory()`

Estimate the memory for the auto.

#### Usage

    AutoLightgbm$estimate_memory(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### Method `internal_measure()`

Get the internal measure for the auto.

#### Usage

    AutoLightgbm$internal_measure(measure, task)

#### Arguments

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)).

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoLightgbm$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
