#!/usr/bin/env Rscript
# chmod +x learner_mlp.R
# Rscript learner_mlp.R <nrow> <nfeatures> <n_layers> <neurons> <p> <opt.lr> <opt.weight_decay> <epochs>

args = commandArgs(trailingOnly = TRUE)

if (length(args) != 11L) {
  stop("Usage: learner_ftt.R <nrow> <nfeatures> <n_blocks> <d_token> <residual_dropout> <attention_dropout> <ffn_dropout> <ffn_d_hidden_multiplier> <opt.lr> <opt.weight_decay> <epochs>\n",
       call. = FALSE)
}

args = lapply(args, as.numeric)
names(args) = c("nrow", "nfeatures",
                "n_blocks", "d_token", "residual_dropout", "attention_dropout", "ffn_dropout", "ffn_d_hidden_multiplier",
                "opt.lr", "opt.weight_decay", "epochs")


if (any(is.na(args))) {
  stop("Arguments missing\n", call. = FALSE)
}

library(mlr3verse)
library(mlr3misc)
library(mlr3torch)

# binary classification

task = tsk("spam")
task$filter(sample(task$row_ids, args$nrow, replace = TRUE))
task$select(sample(task$feature_names, args$nfeatures, replace = TRUE))


# functions for FTTRansformer
no_wd = function(name) {
  # this will also disable weight decay for the input projection bias of the attention heads
  no_wd_params = c("_normalization", "bias")

  return(any(map_lgl(no_wd_params, function(pattern) grepl(pattern, name, fixed = TRUE))))
}

rtdl_param_groups = function(parameters) {
  split_param_names = strsplit(names(parameters), ".", fixed = TRUE)

  ffn_norm_idx = grepl("ffn_normalization", names(parameters), fixed = TRUE)
  first_ffn_norm_num_in_module_list = as.integer(split_param_names[ffn_norm_idx][[1]][2])
  cls_num_in_module_list = first_ffn_norm_num_in_module_list - 1
  nums_in_module_list = sapply(split_param_names, function(x) as.integer(x[2]))
  tokenizer_idx = nums_in_module_list < cls_num_in_module_list

  # the last normalization layer is unnamed, so we need to find it based on its position in the module list
  last_module_num_in_module_list = as.integer(split_param_names[[length(split_param_names)]][2])
  last_norm_num_in_module_list = last_module_num_in_module_list - 2
  last_norm_idx = nums_in_module_list == last_norm_num_in_module_list

  no_wd_idx = map_lgl(names(parameters), no_wd) | tokenizer_idx | last_norm_idx
  no_wd_group = parameters[no_wd_idx]

  main_group = parameters[!no_wd_idx]

  list(
    list(params = main_group),
    list(params = no_wd_group, weight_decay = 0)
  )
}

learner = lrn(
  sprintf("%s.ft_transformer", task$task_type),
  id = "ft_transformer",
  # measures_valid = measure,
  # patience = self$early_stopping_rounds(task),  # no early stopping
  batch_size = 32L,
  attention_n_heads = 8L,
  opt.param_groups = rtdl_param_groups,
  device = "auto",  # in "cuda", "auto"

  # parameters to test memory usage
  n_blocks = as.integer(args$n_blocks),
  d_token = as.integer(args$d_token),
  residual_dropout = args$residual_dropout,
  attention_dropout = args$attention_dropout,
  ffn_dropout = args$ffn_dropout,
  ffn_d_hidden_multiplier = args$ffn_d_hidden_multiplier,
  opt.lr = args$opt.lr,
  opt.weight_decay = args$opt.weight_decay,
  epochs = args$epochs
)

# train and predict once
ids = partition(task)
learner$train(task, row_ids = ids$train)
preds = learner$predict(task, row_ids = ids$test)

