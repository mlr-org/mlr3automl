
# Build hyperparameter tables ####

# nrow = 10 ^ (1:3)  
# nfeatures = c(1, 10, 20, 40)
# n_layers = 2 ^ (1:4)  # in 2, 16
# neurons = 2 ^ (1:10)  # in 2, 1024
# p = seq(0, 0.5, 0.1)  # in 0,0.5
# opt.lr = c(1e-5, 1e-2)
# opt.weight_decay = c(1e-6, 1e-3)
# epochs = c(1L, 10L, 100L)  # in 1, 100

# "easy" hypothesis (should not have an influence) ####

# data: 4000 x 50 to see small differences
# log scale because param scale is a log scale
lr = exp(seq(log(1e-5), log(1e-2), length.out = 10))
weight_decay = exp(seq(log(1e-6), log(1e-3), length.out = 10))

easy_lr = data.frame(
  nrow = 10000L,
  nfeatures = 50L,
  n_layers = 4L, # default from AutoMlp
  neurons = 128L, # default from AutoMlp
  p = 0.2, # default from AutoMlp
  opt_lr = lr,
  opt_weight_decay = 1e-6,  # default from AutoMlp
  epochs = 10
)

easy_weight_decay = data.frame(
  nrow = 10000L,
  nfeatures = 50L,
  n_layers = 4L, # default from AutoMlp
  neurons = 128L, # default from AutoMlp
  p = 0.2, # default from AutoMlp
  opt_lr = 1e-5,  # default from AutoMlp
  opt_weight_decay = weight_decay,
  epochs = 10
)

write.csv(rbind(easy_lr, easy_weight_decay), file = "parameters_mlp_easy.csv")

# p: should only be 0 vs. not 0
p_or_not = data.frame(
  nrow = 10000L,
  nfeatures = 50L,
  n_layers = 4L, # default from AutoMlp
  neurons = 128L, # default from AutoMlp
  p =  seq(0, 0.5, 0.1),
  opt_lr = 1e-5,  # default from AutoMlp
  opt_weight_decay = 1e-6,  # default from AutoMlp
  epochs = 10
)

write.csv(p_or_not, file = "parameters_mlp_p.csv")


# Influence of size
data_size = expand.grid(
  nrow      = c(10, 100, 1000, 5000, 10000, 50000, 100000, 500000),
  nfeatures = c(1, 10, 50, 100, 500, 1000)
)

data_size = transform(
  data_size,
  n_layers = 4L, # default from AutoMlp
  neurons = 128L, # default from AutoMlp
  p = 0.2, # default from AutoMlp
  opt_lr = 1e-5,  # default from AutoMlp
  opt_weight_decay = 1e-6,  # default from AutoMlp
  epochs = 10
)

write.csv(data_size, file = "parameters_mlp_size.csv")

# Influence of neurons and layers
layer_neuron = expand.grid(
  n_layers = 2 ^ (1:4),
  neurons =  2 ^ (1:10)
)

layer_neuron = transform(
  layer_neuron, 
  nrow = 10000L,
  nfeatures = 50L,
  p = 0.2, # default from AutoMlp
  opt_lr = 1e-5,  # default from AutoMlp
  opt_weight_decay = 1e-6,  # default from AutoMlp
  epochs = 10
)

layer_neuron = layer_neuron[
  c("nrow", "nfeatures", "n_layers", "neurons", "p", "opt_lr", "opt_weight_decay", "epochs") 
]

write.csv(layer_neuron, file = "parameters_mlp_neurons-layer.csv")


# Influence of epochs

epochs = data.frame(
  nrow = 10000L,
  nfeatures = 50L,
  n_layers = 4L, # default from AutoMlp
  neurons = 128L, # default from AutoMlp
  p =  0.2,
  opt_lr = 1e-5,  # default from AutoMlp
  opt_weight_decay = 1e-6,  # default from AutoMlp
  epochs = c(1, 10, 20, 100, 200)
)

write.csv(epochs, file = "parameters_mlp_epochs.csv")


# all options (but very many)

all_params = expand.grid(
  nrow      = c(10, 100, 1000, 10000, 100000),
  nfeatures = c(1, 10, 100, 1000),
  n_layers = 2 ^ (1:4),
  neurons =  2 ^ (1:10),
  p =  c(0, 0.1, 0.25, 0.5),
  opt_lr = exp(seq(log(1e-5), log(1e-2), length.out = 5)),
  opt_weight_decay = exp(seq(log(1e-6), log(1e-3), length.out = 5)),
  epochs = c(1, 10, 100)
)

write.csv(all_params, file = "parameters_mlp_all.csv")

# grid with nrow, n_layers, neurons
param_grid = expand.grid(
  nrow = floor(10^seq(log10(10), log10(1e6), length.out = 20)),  # log-spaced from 10 1e6
  nfeatures = 50,
  n_layers = c(2, 4, 8, 16),
  neurons  = 2^(1:10),
  p =  0.2,
  opt_lr = 1e-5,  # default from AutoMlp
  opt_weight_decay = 1e-6,  # default from AutoMlp
  epochs = 10
)

write.csv(param_grid, file = "parameters_mlp_lm.csv")
