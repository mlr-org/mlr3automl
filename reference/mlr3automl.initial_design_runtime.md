# Initial Design Runtime Limit Callback

This
[mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html)
fails the tasks of the initial design if 25% of the runtime limit is
reached.

## Examples

``` r
clbk("mlr3automl.initial_design_runtime")
#> <CallbackAsyncTuning:mlr3automl.initial_design_runtime>: Initial Design Runtime Limit Callback
#> * Active Stages: on_optimizer_queue_after_eval
```
