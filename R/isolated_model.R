# wrap models of python learners that should not be loaded in the main process
# learners based on python torch (via reticulate) cannot share a process with mlr3torch
# because the bundled libtorch versions are incompatible
# we overwrite marshal_model() and unmarshal_model()
# because unpickling the model in the main process would load python there
new_isolated_model = function(model) {
  class(model) = c("isolated_model", class(model))
  model
}

#' @export
marshal_model.isolated_model = function(model, inplace = FALSE, ...) {
  class(model) = setdiff(class(model), "isolated_model")
  structure(
    list(marshaled = marshal_model(model, inplace = inplace, ...), packages = "mlr3automl"),
    class = c("isolated_model_marshaled", "marshaled")
  )
}

#' @export
unmarshal_model.isolated_model_marshaled = function(model, inplace = FALSE, ...) {
  structure(list(pickled = model$marshaled), class = "isolated_model_pickled")
}

#' @export
marshal_model.isolated_model_pickled = function(model, inplace = FALSE, ...) {
  structure(
    list(marshaled = model$pickled, packages = "mlr3automl"),
    class = c("isolated_model_marshaled", "marshaled")
  )
}
