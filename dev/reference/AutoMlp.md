# Mlp Auto

Mlp auto.

## Value

Object of class
[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) and
`AutoMlp`.

## Super class

[`Auto`](https://mlr3automl.mlr-org.com/dev/reference/Auto.md) -\>
`AutoMlp`

## Methods

### Public methods

- [`AutoMlp$new()`](#method-AutoMlp-initialize)

- [`AutoMlp$graph()`](#method-AutoMlp-graph)

- [`AutoMlp$estimate_memory()`](#method-AutoMlp-estimate_memory)

- [`AutoMlp$clone()`](#method-AutoMlp-clone)

Inherited methods

- [`Auto$check()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-check)
- [`Auto$design_default()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_default)
- [`Auto$design_set()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_set)
- [`Auto$early_stopping_rounds()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-early_stopping_rounds)
- [`Auto$finalize_model()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-finalize_model)
- [`Auto$search_space()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-search_space)

------------------------------------------------------------------------

### `AutoMlp$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoMlp$new(id = "mlp")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### `AutoMlp$graph()`

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

### `AutoMlp$estimate_memory()`

Estimate the memory for the auto.

#### Usage

    AutoMlp$estimate_memory(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoMlp$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoMlp$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
auto("mlp")
#> <AutoMlp>
#>   Inherits from: <Auto>
#>   Public:
#>     check: function (task, memory_limit = Inf, large_data_set = FALSE, devices) 
#>     clone: function (deep = FALSE) 
#>     design_default: function (task) 
#>     design_set: function (task, measure, size) 
#>     devices: cuda cpu
#>     early_stopping_rounds: function (task, budget = Inf) 
#>     estimate_memory: function (task) 
#>     finalize_model: function (graph_learner) 
#>     graph: function (task, measure, n_threads, timeout, devices) 
#>     id: mlp
#>     initialize: function (id = "mlp") 
#>     packages: mlr3 mlr3torch
#>     properties: internal_tuning
#>     search_space: function (task) 
#>     task_types: classif regr
#>   Private:
#>     .default_values: list
#>     .search_space: ParamSet, R6
```
