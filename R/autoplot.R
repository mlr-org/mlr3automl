#' @title Plots for Auto Learners
#'
#' @return [ggplot2::ggplot()].
#'
#' @export
autoplot.LearnerClassifAuto = function(object, type = "marginal", split_branch = TRUE, add_arrow = TRUE, cols_x = NULL, trafo = FALSE, grid_resolution = 100, batch = NULL, theme = theme_minimal(), ...) { # nolint
  assert_flag(trafo)

  require_namespaces("mlr3viz")

  object = object$model$tuning_instance

  if (is.null(cols_x)) {
    cols_x = if (trafo) {
      paste0("x_domain_", object$archive$cols_x)
    } else {
      object$archive$cols_x
    }
  }

  if (type %in% c("pca", "hyperparameter")) {
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

        plot = ggplot(data_dim,
          mapping = aes(x = PC1,
            y = PC2)) +
          geom_point(
            mapping = aes(fill = data[[cols_y]]),
            data = data_dim,
            shape = 21,
            size = 3,
            alpha = 0.5) +
          geom_point(
            data = data_dim[1, ],
            mapping = aes(x = PC1,
              y = PC2),
            shape = 21,
            colour = "green",
            alpha = 1,
            size = 5) +
          geom_point(
            data = data_dim[nrow(data_dim), ],
            mapping = aes(x = PC1,
              y = PC2),
            shape = 21,
            colour = "red",
            alpha = 1,
            size = 5) +
          labs(
            x = "First Principal Component",
            y = "Second Principal Component",
            fill = cols_y
          ) +
          scale_fill_viridis_c() +
          guides(fill = guide_colorbar(barwidth = 0.5, barheight = 10)) +
          theme
        if (add_arrow) {
          plot = plot +
            geom_segment(
              aes(xend = c(tail(data_dim$PC1, n = -1), NA),
                yend = c(tail(data_dim$PC2, n = -1), NA)),
              arrow = arrow(length = unit(0.2, "cm")))
        }
        return(plot)
      },
      "hyperparameter" = {
        data = data[, c(..cols_x, ..cols_y)]
        task = TaskRegr$new(id = "viz", backend = data, target = cols_y)
        lrn = lrn("regr.rpart", keep_model = TRUE)
        lrn = as_learner(pipeline_robustify(task, lrn) %>>% po("learner", lrn))
        lrn$train(task)
        tree = lrn$graph_model$pipeops$regr.rpart$learner_model

        plot = ggparty::ggparty(partykit::as.party(tree$model)) +
          ggparty::geom_edge() +
          ggparty::geom_edge_label() +
          ggparty::geom_node_splitvar() +
          ggparty::geom_node_plot(
            gglist = list(
              geom_violin(aes(x = "", y = .data[[cols_y]])),
              xlab(cols_y),
              scale_fill_viridis_d(end = 0.8),
              theme),
            ids = "terminal",
            shared_axis_labels = TRUE) +
          ggparty::geom_node_label(
            mapping = aes(label = paste0("n=", .data[["nodesize"]])),
            nudge_y = 0.03,
            ids = "terminal")
        return(plot)
      },

      stopf("Unknown plot type '%s'", type)
    )
  } else {
    branch = object$result$branch.selection
    cols_x = cols_x[grepl(branch, cols_x)]
    if (length(cols_x) == 0) {
      cols_x = object$archive$cols_x
    }
    autoplot(object = object, type = type, cols_x = cols_x, trafo = trafo, learner = learner, grid_resolution = grid_resolution, theme = theme, ...)
  }
}

#' @export
autoplot.LearnerRegrAuto = autoplot.LearnerClassifAuto
