# Drop-in replacement for PipeOpFixFactors that preserves the feature order of the incoming task.
# Remove it again once mlr3pipelines fixes the order drift upstream.
#
# The upstream $.transform() only rebuilds columns whose levels differ from the ones recorded in $state$levels,
# and it re-attaches them with task$select(...)$cbind(...), which moves them to the end of the feature order.
# The training task never needs an adjustment, because the state was derived from that very data,
# so its order stays untouched.
# A task that does need one gets its factors moved to the back,
# and task$internal_valid_task is exactly such a task,
# because PipeOpTaskPreproc pushes it through $.predict() while the graph trains.
# PipeOpTaskPreproc does not notice, because its layout assertion compares feature_types with
# all.equal(..., ignore.row.order = TRUE).
#
# Learners that build their early stopping set from the validation task's own feature_names inherit the permutation.
# catboost aborts with "Feature #N has '<a>' name in training data, but '<b>' name in test dataset #0",
# and lightgbm trains silently on the permuted matrix,
# which corrupts the early stopping metric and the internally tuned num_iterations.
# The trigger is any factor column whose level set differs between the training and validation rows,
# such as a character column converted with as.factor().
PipeOpFixFactorsStable = R6Class("PipeOpFixFactorsStable",
  inherit = PipeOpFixFactors,
  public = list(
    initialize = function(id = "fixfactors", param_vals = list()) {
      super$initialize(id = id, param_vals = param_vals)
    }
  ),

  private = list(
    .transform = function(task) {
      feature_order = task$feature_names
      task = super$.transform(task)
      # the level fixing leaves the column set unchanged, so feature_order is always a valid assignment
      if (!identical(task$feature_names, feature_order)) {
        task$col_roles$feature = feature_order
      }
      task
    }
  )
)
