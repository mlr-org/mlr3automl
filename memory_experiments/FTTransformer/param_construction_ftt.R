
# Build hyperparameter tables ####

# HP ranges from AutoFTTransformer
# n_blocks                = 1:6
# d_token                 = 8 * 2 ^(3:6)
# residual_dropout        = seq(0, 0.2, by = 0.1)
# attention_dropout       = seq(0, 0.5, by = 0.1)
# ffn_dropout             = seq(0, 0.5, by = 0.1)
# ffn_d_hidden_multiplier = 1/3 * c(2, 4, 8)
# opt.lr                  = exp(seq(log(1e-5), log(1e-4), length.out = 4))
# opt.weight_decay        = exp(seq(log(1e-6), log(1e-3), length.out = 10))
# epochs                  = c(1, 10, 50, 100)


# # default values from AutoFTTransformer
df_default = data.frame(
  nrow = 1000, 
  nfeatures = 2,
  n_blocks = 4L,
  d_token = 64L,
  residual_dropout = 0.1,
  attention_dropout = 0.2,
  ffn_dropout = 0.2,
  ffn_d_hidden_multiplier = 2 / 3,
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = 10 # ??
)

write.csv(df_default, file = "parameters_ftt_test.csv")



# should not have an influence ####

# data: 10000 x 10 to see small differences
# for parameters not of interest: use default from AutoFTTransformer
# log scale if param scale is a log scale

# dropouts
df_residual_dropout = data.frame(
  nrow = 10000L,
  nfeatures = 10L,
  n_blocks = 4L,
  d_token = 64L,
  residual_dropout = seq(0, 0.2, by = 0.1),
  attention_dropout = 0.2,
  ffn_dropout = 0.2,
  ffn_d_hidden_multiplier = 2 / 3,
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = 10
)

df_attention_dropout = data.frame(
  nrow = 10000L,
  nfeatures = 10L,
  n_blocks = 4L,
  d_token = 64L,
  residual_dropout = 0.1,
  attention_dropout = seq(0, 0.5, by = 0.1),
  ffn_dropout = 0.2,
  ffn_d_hidden_multiplier = 2 / 3,
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = 10
)

df_ffn_dropout = data.frame(
  nrow = 10000L,
  nfeatures = 10L,
  n_blocks = 4L,
  d_token = 64L,
  residual_dropout = 0.1,
  attention_dropout = 0.2,
  ffn_dropout = seq(0, 0.5, by = 0.1),
  ffn_d_hidden_multiplier = 2 / 3,
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = 10
)

# opt parameters 
lr = exp(seq(log(1e-5), log(1e-4), length.out = 4))
weight_decay = exp(seq(log(1e-6), log(1e-3), length.out = 10))

df_lr = data.frame(
  nrow = 10000L,
  nfeatures = 10L,
  n_blocks = 4L,
  d_token = 64L,
  residual_dropout = 0.1,
  attention_dropout = 0.2,
  ffn_dropout = 0.2,
  ffn_d_hidden_multiplier = 2 / 3,
  lr = lr,
  weight_decay = 1e-6,
  epochs = 10
)

df_weight_decay = data.frame(
  nrow = 10000L,
  nfeatures = 10L,
  n_blocks = 4L,
  d_token = 64L,
  residual_dropout = 0.1,
  attention_dropout = 0.2,
  ffn_dropout = 0.2,
  ffn_d_hidden_multiplier = 2 / 3,
  lr = 1e-5,
  weight_decay = weight_decay,
  epochs = 10
)


# Influence of epochs

df_epochs = data.frame(
  nrow = 10000L,
  nfeatures = 10L,
  n_blocks = 4L,
  d_token = 64L,
  residual_dropout = 0.1,
  attention_dropout = 0.2,
  ffn_dropout = 0.2,
  ffn_d_hidden_multiplier = 2 / 3,
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = c(1, 10, 50, 100)
)

write.csv(rbind(df_residual_dropout, df_attention_dropout, df_ffn_dropout, df_lr, df_weight_decay, df_epochs),
          file = "parameters_ftt_no_influence.csv")



# Influence of size
data_size = expand.grid(
  nrow      = c(10, 100, 1000, 10000, 100000),
  nfeatures = c(1, 10, 50, 100, 500, 1000)
)

df_data_size = transform(
  data_size,
  n_blocks = 4L,
  d_token = 64L,
  residual_dropout = 0.1,
  attention_dropout = 0.2,
  ffn_dropout = 0.2,
  ffn_d_hidden_multiplier = 2 / 3,
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = 10
)

write.csv(df_data_size, file = "parameters_ftt_size.csv")



# Influence of n_blocks
df_n_blocks = data.frame(
  nrow = 10000L,
  nfeatures = 10L,
  n_blocks = as.integer(1:6),
  d_token = 64L,
  residual_dropout = 0.1,
  attention_dropout = 0.2,
  ffn_dropout = 0.2,
  ffn_d_hidden_multiplier = 2 / 3,
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = 10
)

write.csv(df_n_blocks, file = "parameters_ftt_blocks.csv")


# Influence of d_token and ffn_d_hidden_multiplier
# (should have some kind of interaction)

token_grid = expand.grid(
  d_token = 8 * 2 ^(3:6),
  ffn_d_hidden_multiplier = 1/3 * c(2, 4, 8)
)

df_token = transform(
  token_grid, 
  nrow = 10000L,
  nfeatures = 50L,
  n_blocks = 4L,
  residual_dropout = 0.1,
  attention_dropout = 0.2,
  ffn_dropout = 0.2,
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = 10
)

df_token = df_token[
  c("nrow", "nfeatures", "n_blocks", "d_token", "residual_dropout", "attention_dropout", "ffn_dropout", "ffn_d_hidden_multiplier", "lr", "weight_decay", "epochs") 
]

write.csv(df_token, file = "parameters_ftt_token.csv")

# grid with influencial variables (to fix a model ...)
param_grid = expand.grid(
  nrow = c(100, 1000, 5000, 10000, 50000, 100000),  #  floor(10^seq(log10(100), log10(1e5), length.out = 6)),  # log-spaced from 10 1e6, 
  nfeatures = c(1, 10, 50, 100, 500, 1000),
  n_blocks = 4L,
  d_token = 8 * 2 ^(3:6),
  residual_dropout = 0.1,
  attention_dropout = 0.2,
  ffn_dropout = 0.2,
  ffn_d_hidden_multiplier = 1/3 * c(2, 4, 6, 8),
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = 10
)

write.csv(param_grid, file = "parameters_ftt_grid.csv")
