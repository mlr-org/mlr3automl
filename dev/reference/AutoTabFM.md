# TabFM Auto

Tabfm auto.

Tabfm predicts in context, so every prediction runs the backbone over
the training rows once per estimator. This is too slow to be useful on
the CPU, so the auto is registered for `"cuda"` only and
[Auto](https://mlr3automl.mlr-org.com/dev/reference/Auto.md)`$check()`
removes it from the search space whenever `devices` does not include
`"cuda"`. Construct it with `AutoTabFM$new(devices = c("cpu", "cuda"))`
and re-register it in
[mlr_auto](https://mlr3automl.mlr-org.com/dev/reference/mlr_auto.md) to
run it on the CPU anyway.

## Value

Object of class
[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) and
`AutoTabFM`.

## Python learners

Python learners like `TabPFN`, `TabFM`, and `fastai` run via
`reticulate` and therefore need a Python installation with their
required packages. There are two ways to provide it:

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
`AutoTabFM`

## Methods

### Public methods

- [`AutoTabFM$new()`](#method-AutoTabFM-initialize)

- [`AutoTabFM$check()`](#method-AutoTabFM-check)

- [`AutoTabFM$graph()`](#method-AutoTabFM-graph)

- [`AutoTabFM$estimate_memory()`](#method-AutoTabFM-estimate_memory)

- [`AutoTabFM$design_default()`](#method-AutoTabFM-design_default)

- [`AutoTabFM$search_space()`](#method-AutoTabFM-search_space)

- [`AutoTabFM$clone()`](#method-AutoTabFM-clone)

Inherited methods

- [`Auto$design_set()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_set)
- [`Auto$early_stopping_rounds()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-early_stopping_rounds)
- [`Auto$finalize_model()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-finalize_model)

------------------------------------------------------------------------

### `AutoTabFM$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoTabFM$new(id = "tabfm", devices = "cuda")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `devices`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Devices the auto is allowed to run on. Defaults to `"cuda"` only,
  because tabfm is too slow to be useful on the CPU.

------------------------------------------------------------------------

### `AutoTabFM$check()`

Check if the auto is compatible with the task.

#### Usage

    AutoTabFM$check(
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

### `AutoTabFM$graph()`

Create the graph for the auto.

#### Usage

    AutoTabFM$graph(task, measure, n_threads, timeout, devices)

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

### `AutoTabFM$estimate_memory()`

Estimate the memory for the auto.

#### Usage

    AutoTabFM$estimate_memory(task, devices = "cpu")

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

- `devices`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Devices to use. Allowed values are `"cpu"` and `"cuda"`. Default is
  "cpu".

------------------------------------------------------------------------

### `AutoTabFM$design_default()`

Default hyperparameters for the learner.

#### Usage

    AutoTabFM$design_default(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoTabFM$search_space()`

Get the search space for the auto.

#### Usage

    AutoTabFM$search_space(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoTabFM$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoTabFM$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
auto("tabfm")
#> <AutoTabFM>
#>   Inherits from: <Auto>
#>   Public:
#>     check: function (task, memory_limit = Inf, large_data_set = FALSE, devices = "cpu") 
#>     clone: function (deep = FALSE) 
#>     design_default: function (task) 
#>     design_set: function (task, measure, size) 
#>     devices: cuda
#>     early_stopping_rounds: function (task, budget = Inf) 
#>     estimate_memory: function (task, devices = "cpu") 
#>     finalize_model: function (graph_learner) 
#>     graph: function (task, measure, n_threads, timeout, devices) 
#>     id: tabfm
#>     initialize: function (id = "tabfm", devices = "cuda") 
#>     n_cpu: 1
#>     n_gpu: 1
#>     packages: mlr3 mlr3extralearners callr
#>     properties: 
#>     search_space: function (task) 
#>     task_types: classif regr
#>   Private:
#>     .default_values: list
#>     .search_space: ParamSet, R6
```
