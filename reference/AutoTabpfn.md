# Tabpfn Auto

Tabpfn auto.

## Super class

[`mlr3automl::Auto`](https://mlr3automl.mlr-org.com/reference/Auto.md)
-\> `AutoTabpfn`

## Methods

### Public methods

- [`AutoTabpfn$new()`](#method-AutoTabpfn-new)

- [`AutoTabpfn$check()`](#method-AutoTabpfn-check)

- [`AutoTabpfn$graph()`](#method-AutoTabpfn-graph)

- [`AutoTabpfn$estimate_memory()`](#method-AutoTabpfn-estimate_memory)

- [`AutoTabpfn$search_space()`](#method-AutoTabpfn-search_space)

- [`AutoTabpfn$clone()`](#method-AutoTabpfn-clone)

Inherited methods

- [`mlr3automl::Auto$design_default()`](https://mlr3automl.mlr-org.com/reference/Auto.html#method-design_default)
- [`mlr3automl::Auto$design_set()`](https://mlr3automl.mlr-org.com/reference/Auto.html#method-design_set)
- [`mlr3automl::Auto$early_stopping_rounds()`](https://mlr3automl.mlr-org.com/reference/Auto.html#method-early_stopping_rounds)
- [`mlr3automl::Auto$finalize_model()`](https://mlr3automl.mlr-org.com/reference/Auto.html#method-finalize_model)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoTabpfn$new(id = "tabpfn")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### Method `check()`

Check if the auto is compatible with the task.

#### Usage

    AutoTabpfn$check(
      task,
      memory_limit = Inf,
      large_data_set = FALSE,
      devices = "cpu"
    )

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

- `memory_limit`:

  (`integer(1)`).

- `large_data_set`:

  (`logical(1)`).

- `devices`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Devices to use. Allowed values are `"cpu"` and `"cuda"`. Default is
  "cpu".

------------------------------------------------------------------------

### Method `graph()`

Create the graph for the auto.

#### Usage

    AutoTabpfn$graph(task, measure, n_threads, timeout, devices)

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

    AutoTabpfn$estimate_memory(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### Method `search_space()`

Get the search space for the auto.

#### Usage

    AutoTabpfn$search_space(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoTabpfn$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
