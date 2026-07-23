
# Build hyperparameter tables ####

# HP ranges from AutoResNet
# n_blocks            = as.integer(2 ^ (0:4)),
# d_block             = as.integer(2 ^ (6:10)),
# d_hidden_multiplier = as.integer(1:4),
# dropout1            = seq(0, 0.5, by = 0.1),
# dropout2            = seq(0, 0.5, by = 0.1),
# lr                  = exp(seq(log(1e-5), log(1e-2), length.out = 10))
# weight_decay        = exp(seq(log(1e-6), log(1e-3), length.out = 10)),
# epochs              = c(1, 10, 100)

# # default values from AutoResNet
# n_blocks = 4L,
# d_block = 128L,
# d_hidden_multiplier = 1L,
# dropout1 = 0.2,
# dropout2 = 0.2,
# lr = 1e-5,
# weight_decay = 1e-6,

# test bash script with default values
df_default = data.frame(
  nrow = 1000, 
  nfeatures = 2,
  n_blocks = 4L,
  d_block = 128L,
  d_hidden_multiplier = 1L,
  dropout1 = 0.2,
  dropout2 = 0.2,
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = 10 # ??
)

write.csv(df_default, file = "parameters_rn_test.csv")



# should not have an influence ####

# data: 10000 x 10 to see small differences
# for parameters not of interest: use default from AutoResNet
# log scale if param scale is a log scale

# dropouts
df_do1 = data.frame(
  nrow = 10000L,
  nfeatures = 10L,
  n_blocks = 4L,
  d_block = 128L,
  d_hidden_multiplier = 1L,
  dropout1 = seq(0, 0.5, by = 0.1),
  dropout2 = 0.2,
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = 10 # ??
)

df_do2 = data.frame(
  nrow = 10000L,
  nfeatures = 10L,
  n_blocks = 4L,
  d_block = 128L,
  d_hidden_multiplier = 1L,
  dropout1 = 0.2,
  dropout2 = seq(0, 0.5, by = 0.1),
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = 10 # ??
)

# opt parameters 
lr = exp(seq(log(1e-5), log(1e-2), length.out = 10))
weight_decay = exp(seq(log(1e-6), log(1e-3), length.out = 10))

df_lr = data.frame(
  nrow = 10000L,
  nfeatures = 10L,
  n_blocks = 4L,
  d_block = 128L,
  d_hidden_multiplier = 1L,
  dropout1 = 0.2,
  dropout2 = 0.2,
  lr = lr,
  weight_decay = 1e-6,
  epochs = 10
)

df_weight_decay = data.frame(
  nrow = 10000L,
  nfeatures = 10L,
  n_blocks = 4L,
  d_block = 128L,
  d_hidden_multiplier = 1L,
  dropout1 = 0.2,
  dropout2 = 0.2,
  lr = 1e-5,
  weight_decay = weight_decay,
  epochs = 10
)


# Influence of epochs

df_epochs = data.frame(
  nrow = 10000L,
  nfeatures = 10L,
  n_blocks = 4L,
  d_block = 128L,
  d_hidden_multiplier = 1L,
  dropout1 = 0.2,
  dropout2 = 0.2,
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = c(1, 10, 50, 100)
)

write.csv(rbind(df_do1, df_do2, df_lr, df_weight_decay, df_epochs),
          file = "parameters_rn_no_influence.csv")



# Influence of size
data_size = expand.grid(
  nrow      = c(10, 100, 1000, 10000, 100000),
  nfeatures = c(1, 10, 50, 100, 500, 1000)
)

df_data_size = transform(
  data_size,
  n_blocks = 4L,
  d_block = 128L,
  d_hidden_multiplier = 1L,
  dropout1 = 0.2,
  dropout2 = 0.2,
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = 10
)

write.csv(df_data_size, file = "parameters_rn_size.csv")



# Influence of n_blocks
df_n_blocks = data.frame(
  nrow = 10000L,
  nfeatures = 10L,
  n_blocks = as.integer(2 ^ (0:4)),
  d_block = 128L,
  d_hidden_multiplier = 1L,
  dropout1 = 0.2,
  dropout2 = 0.2,
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = 10
)

write.csv(df_n_blocks, file = "parameters_rn_blocks.csv")


# Influence of d_block and d_hidden_multiplier
# (should have some kind of interaction)

d_grid = expand.grid(
  d_block = as.integer(2 ^ (6:10)),
  d_hidden_multiplier = as.integer(1:4)
)

df_d = transform(
  d_grid, 
  nrow = 10000L,
  nfeatures = 50L,
  n_blocks = 4L,
  dropout1 = 0.2,
  dropout2 = 0.2,
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = 10
)

df_d = df_d[
  c("nrow", "nfeatures", "n_blocks", "d_block", "d_hidden_multiplier", "dropout1", "dropout2", "lr", "weight_decay", "epochs") 
]

write.csv(df_d, file = "parameters_rn_token.csv")

# grid with influencial variables (to fix a model ...)
param_grid = expand.grid(
  nrow = c(100, 1000, 5000, 10000, 50000, 100000),  #  floor(10^seq(log10(100), log10(1e5), length.out = 6)),  # log-spaced from 10 1e6, 
  nfeatures = c(1, 10, 50, 100, 500, 1000),
  n_blocks = 4L,
  d_block = as.integer(2 ^ (6:10)),
  d_hidden_multiplier = as.integer(1:4),
  dropout1 = 0.2,
  dropout2 = 0.2,
  lr = 1e-5,
  weight_decay = 1e-6,
  epochs = 10
)
write.csv(param_grid, file = "parameters_rn_grid.csv")
