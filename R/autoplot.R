#' @title Plots for Auto Learners
#'
#' @return [ggplot2::ggplot()].
#'
#' @export
autoplot.LearnerClassifAuto = function(object, type = "marginal", split_branch = TRUE, cols_x = NULL, trafo = FALSE, grid_resolution = 100, batch = NULL, theme = theme_minimal(), ...) { # nolint
  assert_flag(trafo)

  object = object$model$tuning_instance

  if (is.null(cols_x)) {
    cols_x = if (trafo) {
      paste0("x_domain_", object$archive$cols_x)
    } else {
      object$archive$cols_x
    }
  }

  if (type != "pca") {
    branch = object$result$branch.selection
    cols_x = cols_x[grepl(branch, cols_x)]
    if (length(cols_x) == 0) {
      cols_x = object$archive$cols_x
    }
    autoplot(object = object, type = type, cols_x = cols_x, trafo = trafo, learner = learner, grid_resolution = grid_resolution, theme = theme, ...)
  } else {
    cols_y = object$archive$cols_y
    data = fortify(object)
    if (is.null(batch)) batch = seq_len(object$archive$n_batch)
    assert_subset(batch, seq_len(object$archive$n_batch))
    data = data[list(batch), , on = "batch_nr"]

    switch(type,
     # show all settings in 2D with dimensionality reduction
     "pca" = {
       if (length(cols_x) < 3) {
         stop("Need at least 3 parameters.")
       }
       # remove non numeric columns
       char_cols = names(data)[sapply(data, is.character)]
       cols_x = cols_x[!cols_x %in% char_cols]
       data = data[, c(..cols_x, ..cols_y)]
       # replace NA with default
       for (col in colnames(data)) {
         if (any(is.na(data[, ..col]))) {
           data[is.na(get(col)), (col) := defaults[[col]]]
         }
       }
       # remove zero variance columns
       zero_cols = names(data)[sapply(data, function(x) length(unique(x)) == 1)]
       cols_x = setdiff(cols_x, zero_cols)

       # dimensionality reduction
       data_dim = prcomp(data[, ..cols_x], scale. = TRUE)
       data_dim = as.data.table(data_dim$x)

       ggplot(data_dim,
              mapping = aes(
                x = data_dim$PC1,
                y = data_dim$PC2)) +
         geom_point(
           mapping = aes(fill = data[[cols_y]]),
           data = data_dim,
           shape = 21,
           size = 3,
           stroke = 1) +
         labs(
           x = "First Principal Component",
           y = "Second Principal Component",
           fill = cols_y
         ) +
         scale_fill_viridis_c() +
         guides(fill = guide_colorbar(barwidth = 0.5, barheight = 10)) +
         theme
     },

     stopf("Unknown plot type '%s'", type)
    )
  }
}

#' @export
autoplot.LearnerRegrAuto = autoplot.LearnerClassifAuto
