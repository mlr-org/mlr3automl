# Fastai Auto

Fastai auto.

## Value

Object of class
[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) and
`AutoFastai`.

## Python learners

Python learners like `TabPFN` and `fastai` run via `reticulate` and
therefore need a Python installation with their required packages. There
are two ways to provide it:

1.  Do nothing and let
    [`reticulate::py_require()`](https://rstudio.github.io/reticulate/reference/py_require.html)
    install the required packages into an ephemeral virtual environment
    automatically.

2.  Point the `RETICULATE_PYTHON` environment variable to a Python
    installation that has the required packages installed.

We recommend option 2 when running on many workers, as it avoids the
overhead of downloading and installing the packages on each worker. Use
[`install_python_learners()`](https://mlr3automl.mlr-org.com/dev/reference/install_python_learners.md)
to create a conda environment with the required packages and set
`RETICULATE_PYTHON` to the returned Python binary.

The `TabPFN` learner additionally requires the `TABPFN_TOKEN`
environment variable to download the model weights.

## Super class

[`Auto`](https://mlr3automl.mlr-org.com/dev/reference/Auto.md) -\>
`AutoFastai`

## Methods

### Public methods

- [`AutoFastai$new()`](#method-AutoFastai-initialize)

- [`AutoFastai$check()`](#method-AutoFastai-check)

- [`AutoFastai$graph()`](#method-AutoFastai-graph)

- [`AutoFastai$estimate_memory()`](#method-AutoFastai-estimate_memory)

- [`AutoFastai$internal_measure()`](#method-AutoFastai-internal_measure)

- [`AutoFastai$clone()`](#method-AutoFastai-clone)

Inherited methods

- [`Auto$design_default()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_default)
- [`Auto$design_set()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_set)
- [`Auto$early_stopping_rounds()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-early_stopping_rounds)
- [`Auto$finalize_model()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-finalize_model)
- [`Auto$search_space()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-search_space)

------------------------------------------------------------------------

### `AutoFastai$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoFastai$new(id = "fastai")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### `AutoFastai$check()`

Check if the auto is compatible with the task.

#### Usage

    AutoFastai$check(
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

### `AutoFastai$graph()`

Create the graph for the auto.

#### Usage

    AutoFastai$graph(task, measure, n_threads, timeout, devices)

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

### `AutoFastai$estimate_memory()`

Estimate the memory for the auto.

#### Usage

    AutoFastai$estimate_memory(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoFastai$internal_measure()`

Get the internal measure for the auto.

#### Usage

    AutoFastai$internal_measure(measure, task)

#### Arguments

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)).

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoFastai$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoFastai$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
auto("fastai")
#> <AutoFastai>
#>   Inherits from: <Auto>
#>   Public:
#>     check: function (task, memory_limit = Inf, large_data_set = FALSE, devices = "cpu") 
#>     clone: function (deep = FALSE) 
#>     design_default: function (task) 
#>     design_set: function (task, measure, size) 
#>     devices: cpu cuda
#>     early_stopping_rounds: function (task, budget = Inf) 
#>     estimate_memory: function (task) 
#>     finalize_model: function (graph_learner) 
#>     graph: function (task, measure, n_threads, timeout, devices) 
#>     id: fastai
#>     initialize: function (id = "fastai") 
#>     internal_measure: function (measure, task) 
#>     packages: mlr3 mlr3extralearners callr
#>     properties: internal_tuning
#>     search_space: function (task) 
#>     task_types: classif
#>   Private:
#>     .default_values: list
#>     .search_space: ParamSet, R6
```
