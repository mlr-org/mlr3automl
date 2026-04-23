# Mlp Auto

Mlp auto.

## Super class

[`mlr3automl::Auto`](https://mlr3automl.mlr-org.com/reference/Auto.md)
-\> `AutoMlp`

## Methods

### Public methods

- [`AutoMlp$new()`](#method-AutoMlp-new)

- [`AutoMlp$graph()`](#method-AutoMlp-graph)

- [`AutoMlp$estimate_memory()`](#method-AutoMlp-estimate_memory)

- [`AutoMlp$clone()`](#method-AutoMlp-clone)

Inherited methods

- [`mlr3automl::Auto$check()`](https://mlr3automl.mlr-org.com/reference/Auto.html#method-check)
- [`mlr3automl::Auto$design_default()`](https://mlr3automl.mlr-org.com/reference/Auto.html#method-design_default)
- [`mlr3automl::Auto$design_set()`](https://mlr3automl.mlr-org.com/reference/Auto.html#method-design_set)
- [`mlr3automl::Auto$early_stopping_rounds()`](https://mlr3automl.mlr-org.com/reference/Auto.html#method-early_stopping_rounds)
- [`mlr3automl::Auto$finalize_model()`](https://mlr3automl.mlr-org.com/reference/Auto.html#method-finalize_model)
- [`mlr3automl::Auto$search_space()`](https://mlr3automl.mlr-org.com/reference/Auto.html#method-search_space)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoMlp$new(id = "mlp")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### Method `graph()`

Create the graph for the auto.

#### Usage

    AutoMlp$graph(task, measure, n_threads, timeout, devices)

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

### Method `estimate_memory()`

Estimate the memory for the auto.

#### Usage

    AutoMlp$estimate_memory(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoMlp$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
