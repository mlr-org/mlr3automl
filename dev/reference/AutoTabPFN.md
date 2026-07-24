# TabPFN Auto

Tabpfn auto.

## Value

Object of class
[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) and
`AutoTabPFN`.

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
`AutoTabPFN`

## Methods

### Public methods

- [`AutoTabPFN$new()`](#method-AutoTabPFN-initialize)

- [`AutoTabPFN$check()`](#method-AutoTabPFN-check)

- [`AutoTabPFN$graph()`](#method-AutoTabPFN-graph)

- [`AutoTabPFN$estimate_memory()`](#method-AutoTabPFN-estimate_memory)

- [`AutoTabPFN$design_default()`](#method-AutoTabPFN-design_default)

- [`AutoTabPFN$search_space()`](#method-AutoTabPFN-search_space)

- [`AutoTabPFN$clone()`](#method-AutoTabPFN-clone)

Inherited methods

- [`Auto$design_set()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-design_set)
- [`Auto$early_stopping_rounds()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-early_stopping_rounds)
- [`Auto$finalize_model()`](https://mlr3automl.mlr-org.com/dev/reference/Auto.html#method-finalize_model)

------------------------------------------------------------------------

### `AutoTabPFN$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoTabPFN$new(id = "tabpfn")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### `AutoTabPFN$check()`

Check if the auto is compatible with the task.

#### Usage

    AutoTabPFN$check(
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

### `AutoTabPFN$graph()`

Create the graph for the auto.

#### Usage

    AutoTabPFN$graph(task, measure, n_threads, timeout, devices)

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

### `AutoTabPFN$estimate_memory()`

Estimate the memory for the auto.

#### Usage

    AutoTabPFN$estimate_memory(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoTabPFN$design_default()`

Default hyperparameters for the learner.

#### Usage

    AutoTabPFN$design_default(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoTabPFN$search_space()`

Get the search space for the auto.

#### Usage

    AutoTabPFN$search_space(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### `AutoTabPFN$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoTabPFN$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
auto("tabpfn")
#> <AutoTabPFN>
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
#>     id: tabpfn
#>     initialize: function (id = "tabpfn") 
#>     packages: mlr3 mlr3extralearners callr
#>     properties: 
#>     search_space: function (task) 
#>     task_types: classif regr
#>   Private:
#>     .default_values: list
#>     .search_space: ParamSet, R6
```
