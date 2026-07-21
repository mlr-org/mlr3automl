# Ranger Auto

Ranger auto.

## Value

Object of class
[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) and
`AutoRanger`.

## Super class

[`Auto`](https://mlr3automl.mlr-org.com/dev/reference/Auto.md) -\>
`AutoRanger`

## Methods

### Public methods

- [`AutoRanger$new()`](#method-AutoRanger-initialize)

- [`AutoRanger$graph()`](#method-AutoRanger-graph)

- [`AutoRanger$estimate_memory()`](#method-AutoRanger-estimate_memory)

- [`AutoRanger$clone()`](#method-AutoRanger-clone)

Inherited methods

- [`Auto$check()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-check)
- [`Auto$design_default()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_default)
- [`Auto$design_set()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_set)
- [`Auto$early_stopping_rounds()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-early_stopping_rounds)
- [`Auto$finalize_model()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-finalize_model)
- [`Auto$search_space()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-search_space)

------------------------------------------------------------------------

### `AutoRanger$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoRanger$new(id = "ranger")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### `AutoRanger$graph()`

Create the graph for the auto.

#### Usage

    AutoRanger$graph(task, measure, n_threads, timeout, devices)

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

### `AutoRanger$estimate_memory()`

Estimate the memory for the auto.

#### Usage

    AutoRanger$estimate_memory(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoRanger$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoRanger$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
auto("ranger")
#> <AutoRanger>
#>   Inherits from: <Auto>
#>   Public:
#>     check: function (task, memory_limit = Inf, large_data_set = FALSE, devices) 
#>     clone: function (deep = FALSE) 
#>     design_default: function (task) 
#>     design_set: function (task, measure, size) 
#>     devices: cpu
#>     early_stopping_rounds: function (task, budget = Inf) 
#>     estimate_memory: function (task) 
#>     finalize_model: function (graph_learner) 
#>     graph: function (task, measure, n_threads, timeout, devices) 
#>     id: ranger
#>     initialize: function (id = "ranger") 
#>     packages: mlr3 mlr3learners ranger
#>     properties: large_data_sets
#>     search_space: function (task) 
#>     task_types: classif regr
#>   Private:
#>     .default_values: list
#>     .search_space: ParamSet, R6
```
