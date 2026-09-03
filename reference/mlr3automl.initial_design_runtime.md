# Initial Design Runtime Limit Callback

This
[mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html)
drops the remaining tasks of the initial design from the queue when a
configurable fraction (`initial_design_fraction`) of the runtime limit
is reached. The `initial_design_fraction` is set to `0.25` by default.

## Examples

``` r
clbk("mlr3automl.initial_design_runtime", initial_design_fraction = 0.5)
#> <CallbackAsyncTuning:mlr3automl.initial_design_runtime>: Initial Design Runtime Limit Callback
#> * Active Stages: on_optimizer_queue_after_eval
```
