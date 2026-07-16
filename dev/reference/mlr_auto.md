# Dictionary of Auto Objects

A dictionary of
[Auto](https://mlr3automl.mlr-org.com/dev/reference/Auto.md) objects.

Sugar function to retrieve
[Auto](https://mlr3automl.mlr-org.com/dev/reference/Auto.md) objects
from mlr_auto.

## Usage

``` r
mlr_auto

auto(.key, ...)
```

## Arguments

- .key:

  (`character(1)`)  
  Key of the object to retrieve. If missing, the dictionary itself is
  returned.

- ...:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Additional arguments passed to the constructor.

## Value

[Auto](https://mlr3automl.mlr-org.com/dev/reference/Auto.md)

## Examples

``` r
auto("catboost")
#> <AutoCatboost>
#>   Inherits from: <Auto>
#>   Public:
#>     check: function (task, memory_limit = Inf, large_data_set = FALSE, devices) 
#>     clone: function (deep = FALSE) 
#>     design_default: function (task) 
#>     design_set: function (task, measure, size) 
#>     devices: cpu cuda
#>     early_stopping_rounds: function (task) 
#>     estimate_memory: function (task) 
#>     finalize_model: function (graph_learner) 
#>     graph: function (task, measure, n_threads, timeout, devices) 
#>     id: catboost
#>     initialize: function (id = "catboost") 
#>     internal_measure: function (measure, task) 
#>     packages: mlr3 mlr3extralearners catboost
#>     properties: internal_tuning large_data_sets
#>     search_space: function (task) 
#>     task_types: classif regr
#>   Private:
#>     .default_values: list
#>     .search_space: ParamSet, R6
```
