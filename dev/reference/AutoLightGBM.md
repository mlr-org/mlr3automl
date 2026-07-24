# LightGBM Auto

Lightgbm auto.

## Value

Object of class
[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) and
`AutoLightGBM`.

## Super class

[`Auto`](https://mlr3automl.mlr-org.com/dev/reference/Auto.md) -\>
`AutoLightGBM`

## Methods

### Public methods

- [`AutoLightGBM$new()`](#method-AutoLightGBM-initialize)

- [`AutoLightGBM$graph()`](#method-AutoLightGBM-graph)

- [`AutoLightGBM$finalize_model()`](#method-AutoLightGBM-finalize_model)

- [`AutoLightGBM$estimate_memory()`](#method-AutoLightGBM-estimate_memory)

- [`AutoLightGBM$internal_measure()`](#method-AutoLightGBM-internal_measure)

- [`AutoLightGBM$clone()`](#method-AutoLightGBM-clone)

Inherited methods

- [`Auto$check()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-check)
- [`Auto$design_default()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_default)
- [`Auto$design_set()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_set)
- [`Auto$early_stopping_rounds()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-early_stopping_rounds)
- [`Auto$search_space()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-search_space)

------------------------------------------------------------------------

### `AutoLightGBM$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoLightGBM$new(id = "lightgbm")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### `AutoLightGBM$graph()`

Create the graph for the auto.

#### Usage

    AutoLightGBM$graph(task, measure, n_threads, timeout, devices)

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

### `AutoLightGBM$finalize_model()`

Prepare the graph learner for the final model fit.

#### Usage

    AutoLightGBM$finalize_model(graph_learner)

#### Arguments

- `graph_learner`:

  ([mlr3pipelines::GraphLearner](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_graph.html)).

------------------------------------------------------------------------

### `AutoLightGBM$estimate_memory()`

Estimate the memory for the auto.

#### Usage

    AutoLightGBM$estimate_memory(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoLightGBM$internal_measure()`

Get the internal measure for the auto.

#### Usage

    AutoLightGBM$internal_measure(measure, task)

#### Arguments

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)).

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoLightGBM$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoLightGBM$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
auto("lightgbm")
#> <AutoLightGBM>
#>   Inherits from: <Auto>
#>   Public:
#>     check: function (task, memory_limit = Inf, large_data_set = FALSE, devices) 
#>     clone: function (deep = FALSE) 
#>     design_default: function (task) 
#>     design_set: function (task, measure, size) 
#>     devices: cpu cuda
#>     early_stopping_rounds: function (task, budget = Inf) 
#>     estimate_memory: function (task) 
#>     finalize_model: function (graph_learner) 
#>     graph: function (task, measure, n_threads, timeout, devices) 
#>     id: lightgbm
#>     initialize: function (id = "lightgbm") 
#>     internal_measure: function (measure, task) 
#>     packages: mlr3 mlr3extralearners lightgbm
#>     properties: internal_tuning large_data_sets
#>     search_space: function (task) 
#>     task_types: classif regr
#>   Private:
#>     .default_values: list
#>     .search_space: ParamSet, R6
```
