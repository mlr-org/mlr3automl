# Extra Trees Auto

Extra Trees auto.

## Value

Object of class
[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) and
`AutoExtraTrees`.

## Super class

[`Auto`](https://mlr3automl.mlr-org.com/dev/reference/Auto.md) -\>
`AutoExtraTrees`

## Methods

### Public methods

- [`AutoExtraTrees$new()`](#method-AutoExtraTrees-initialize)

- [`AutoExtraTrees$graph()`](#method-AutoExtraTrees-graph)

- [`AutoExtraTrees$estimate_memory()`](#method-AutoExtraTrees-estimate_memory)

- [`AutoExtraTrees$clone()`](#method-AutoExtraTrees-clone)

Inherited methods

- [`Auto$check()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-check)
- [`Auto$design_default()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_default)
- [`Auto$design_set()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_set)
- [`Auto$early_stopping_rounds()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-early_stopping_rounds)
- [`Auto$finalize_model()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-finalize_model)
- [`Auto$search_space()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-search_space)

------------------------------------------------------------------------

### `AutoExtraTrees$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoExtraTrees$new(id = "extra_trees")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### `AutoExtraTrees$graph()`

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

### `AutoExtraTrees$estimate_memory()`

Estimate the memory for the auto.

#### Usage

    AutoExtraTrees$estimate_memory(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoExtraTrees$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoExtraTrees$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
auto("extra_trees")
#> <AutoExtraTrees>
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
#>     id: extra_trees
#>     initialize: function (id = "extra_trees") 
#>     packages: mlr3 mlr3learners ranger
#>     properties: large_data_sets hyperparameter-free
#>     search_space: function (task) 
#>     task_types: classif regr
#>   Private:
#>     .default_values: list
#>     .search_space: ParamSet, R6
```
