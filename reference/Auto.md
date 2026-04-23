# Auto Class

This class is the base class for all autos.

## Public fields

- `id`:

  (`character(1)`).

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html)).

- `task_types`:

  ([`character()`](https://rdrr.io/r/base/character.html)).

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html)).

- `devices`:

  ([`character()`](https://rdrr.io/r/base/character.html)).

## Methods

### Public methods

- [`Auto$new()`](#method-Auto-new)

- [`Auto$check()`](#method-Auto-check)

- [`Auto$graph()`](#method-Auto-graph)

- [`Auto$early_stopping_rounds()`](#method-Auto-early_stopping_rounds)

- [`Auto$estimate_memory()`](#method-Auto-estimate_memory)

- [`Auto$finalize_model()`](#method-Auto-finalize_model)

- [`Auto$design_default()`](#method-Auto-design_default)

- [`Auto$design_set()`](#method-Auto-design_set)

- [`Auto$search_space()`](#method-Auto-search_space)

- [`Auto$clone()`](#method-Auto-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    Auto$new(
      id,
      properties = character(0),
      task_types = character(0),
      packages = character(0),
      devices = character(0)
    )

#### Arguments

- `id`:

  (`character(1)`).

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html)).

- `task_types`:

  ([`character()`](https://rdrr.io/r/base/character.html)).

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html)).

- `devices`:

  ([`character()`](https://rdrr.io/r/base/character.html)).

------------------------------------------------------------------------

### Method `check()`

Check if the auto is compatible with the task.

#### Usage

    Auto$check(task, memory_limit = Inf, large_data_set = FALSE, devices)

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

    Auto$graph(task, measure, n_threads, timeout, devices)

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

### Method `early_stopping_rounds()`

Estimate the number of early stopping rounds.

#### Usage

    Auto$early_stopping_rounds(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### Method `estimate_memory()`

Estimate the memory for the auto.

#### Usage

    Auto$estimate_memory(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### Method `finalize_model()`

Prepare the graph learner for the final model fit. Called after tuning
to undo tuning-only setup (e.g., timeout callbacks).

#### Usage

    Auto$finalize_model(graph_learner)

#### Arguments

- `graph_learner`:

  ([mlr3pipelines::GraphLearner](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_graph.html)).

------------------------------------------------------------------------

### Method `design_default()`

Default hyperparameters for the learner.

#### Usage

    Auto$design_default(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### Method `design_set()`

Get the initial hyperparameter set for the learner.

#### Usage

    Auto$design_set(task, measure, size)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)).

- `size`:

  (`integer(1)`).

------------------------------------------------------------------------

### Method `search_space()`

Get the search space for the learner.

#### Usage

    Auto$search_space(task)

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Auto$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
