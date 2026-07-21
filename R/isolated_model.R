# runs a private method of a python learner in a fresh callr session.
# learners based on python torch (via reticulate) cannot share a process with the
# torch package (mlr3torch) because the bundled libtorch versions are incompatible.
# mlr3 replicates all loaded mlr3 extension packages in its encapsulation sessions,
# so those sessions are not safe for python either.
# the nested session only loads the namespaces referenced by the learner,
# so libtorch never enters the process that initializes python.
isolated_session = function(learner, task, method) {
  callr::r(
    function(learner, task, method) {
      mlr3misc::get_private(learner)[[method]](task)
    },
    args = list(learner = learner, task = task, method = method)
  )
}

# the model is pickled inside the isolated session and crosses all process
# boundaries as raw bytes wrapped in "isolated_model_pickled".
# unpickling only happens inside the next isolated session,
# because unpickling loads python into the calling process.
# the pure-R marshal/unmarshal methods keep the bytes intact when mlr3
# marshals the model, e.g. for encapsulation or serialization.

#' @export
marshal_model.isolated_model_pickled = function(model, inplace = FALSE, ...) {
  structure(
    list(marshaled = model$pickled, packages = "mlr3automl"),
    class = c("isolated_model_marshaled", "marshaled")
  )
}

#' @export
unmarshal_model.isolated_model_marshaled = function(model, inplace = FALSE, ...) {
  structure(list(pickled = model$marshaled), class = "isolated_model_pickled")
}
