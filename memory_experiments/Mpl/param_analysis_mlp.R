library(tidyverse)
library(broom)

p_or_not = read.csv("memory_experiments/Mlp/memory_result_mlp_p.csv")
easy = read.csv("memory_experiments/Mlp/memory_result_mlp_easy.csv")
data_size = read.csv("memory_experiments/Mlp/memory_result_mlp_size.csv")
layer_neuron = read.csv("memory_experiments/Mlp/memory_result_mlp_neurons-layer.csv")
epochs = read.csv("memory_experiments/Mlp/memory_result_mlp_epochs.csv")

param_grid = read.csv("memory_experiments/Mlp/memory_result_mlp_lm.csv")

empty_session = 455120 / 1024  # mib, only packages
r_session = 460560 / 1024  # mib, load task


p_or_not$real_mib = p_or_not$rss_mib - r_session  # p
easy$real_mib = easy$rss_mib - r_session  # opt_lr, opt_weight_decay
data_size$real_mib = data_size$rss_mib - r_session  # nrow, nfeatures
layer_neuron$real_mib = layer_neuron$rss_mib - r_session  # n_layers, neurons
epochs$real_mib = epochs$rss_mib - r_session  # epochs
param_grid$real_mib = param_grid$rss_mib - r_session  # n

dfs = list(
  p_or_not     = p_or_not,
  easy         = easy,
  data_size    = data_size,
  layer_neuron = layer_neuron,
  epochs       = epochs
)

all_res = bind_rows(dfs, .id = "scenario")


# look at results ####

res_fun = function(data, col) {
  data %>%
    group_by(!!sym(col)) %>%
    summarize(real_memory = mean(real_mib)) %>% 
    arrange(!!sym(col)) %>% 
    mutate(diff = real_memory - lag(real_memory))
}


cols_to_diff = list(
  p_or_not     = "p",
  easy         = c("opt_lr", "opt_weight_decay"),
  data_size    = c("nrow", "nfeatures"),
  layer_neuron = c("n_layers", "neurons"),
  epochs       = "epochs"
)

all_diffs = imap(cols_to_diff, function(cols, ds_name) {
  # for each dataset name, loop over its cols
  map_dfr(cols, function(col) {
    cat("\n\n", col, "\n")
    res = res_fun(dfs[[ds_name]], col)
    print(res)
    res = res %>% 
      mutate(
        variable = col
      )
  })
}) %>% 
  bind_rows()


## see for parameter grid nrow, n_layers, neurns
print(res_fun(param_grid, "nrow"), n = 50)
res_fun(param_grid, "n_layers")
res_fun(param_grid, "neurons")

# descriptive plots (for model specification) ####

# target
ggplot(param_grid, aes(x = real_mib)) + geom_density()
ggplot(param_grid, aes(x = log(real_mib))) + geom_density()


# target vs. features
## nrow -> log 10  transformed
ggplot(param_grid, aes(y = real_mib, x = nrow)) + geom_point() + geom_smooth(method = "lm")
ggplot(param_grid, aes(y = real_mib, x = log(nrow, base = 10))) + geom_point() + geom_smooth(method = "lm")
ggplot(param_grid, aes(y = real_mib, x = log(nrow, base = 10))) + geom_point(aes(color = factor(n_layers))) + geom_smooth(method = "lm") + scale_color_viridis_d(direction = -1) 
ggplot(param_grid, aes(y = real_mib, x = log(nrow, base = 10))) + geom_point(aes(color = factor(neurons))) + geom_smooth(method = "lm") + scale_color_viridis_d(direction = -1) 
ggplot(param_grid, aes(y = real_mib, x = log(nrow, base = 10))) + geom_point(aes(color = factor(neurons))) + geom_smooth(method = "lm") + scale_color_viridis_d(direction = -1) + facet_wrap(~n_layers )

# neurons -> log 2 transformes
ggplot(param_grid, aes(y = real_mib, x = neurons)) + geom_point() + geom_smooth(method = "lm")
ggplot(param_grid, aes(y = real_mib, x = log(neurons, base = 2))) + geom_point() + geom_smooth(method = "lm") # more exponential

ggplot(param_grid, aes(y = real_mib, x = log(neurons, base = 2))) + geom_point(aes(color = nrow), alpha = 0.7) + geom_smooth(method = "lm") + scale_color_viridis_c(direction = -1)
ggplot(param_grid, aes(y = real_mib, x = log(neurons, base = 2))) + geom_point(aes(color = n_layers), alpha = 0.7) + geom_smooth(method = "lm") + scale_color_viridis_c(direction = -1)
ggplot(param_grid, aes(y = real_mib, x = log(neurons, base = 2))) + geom_point(aes(color = nrow), alpha = 0.7) + geom_smooth(method = "lm") + scale_color_viridis_c(direction = -1) + facet_wrap(~n_layers )

# n_layers -> log 2 transformed
ggplot(param_grid, aes(y = real_mib, x = n_layers)) + geom_point() + geom_smooth(method = "lm")
ggplot(param_grid, aes(y = real_mib, x = log(n_layers, base = 2))) + geom_point() + geom_smooth(method = "lm") # more exponential 


# Fit linear model (and log scale? Grid is on log scale ...) ####

# correlation matrix
all_res %>% 
  select(nrow, nfeatures, n_layers, neurons, epochs, p, opt_lr, opt_weight_decay, real_mib) %>%
  GGally::ggpairs()
# - very small correlation with mib:  nfeatures, epochs, p, opt_lr, opt_weight_decay
# - high correlations (interactions!): nrow * nfeatures

param_grid %>% 
  select(nrow, n_layers, neurons, real_mib) %>%
  GGally::ggpairs(param_grid)
# - no correlation between the covariates -> interactions not plausible
# - highish correlation between covariates and real mib

# all variables on normal scale
lm1 = lm(real_mib ~ nrow + nfeatures + n_layers + neurons + p + opt_lr + opt_weight_decay + epochs, data = all_res)

# only variables with highish correlation
lm2 = lm(real_mib ~ nrow + n_layers + neurons, data = all_res)

lm2_grid = lm(real_mib ~ nrow + n_layers + neurons, data = param_grid)
lm2_grid_log = lm(log(real_mib) ~ log(nrow) + log(n_layers) + log(neurons), data = param_grid)
# glm_gamma = glm(real_mib ~ nrow + n_layers + neurons, data = param_grid, family = Gamma(link = "log"))
glm_gamma = glm(rss_mib ~ nrow + n_layers + neurons, data = param_grid, family = Gamma(link = "log"))


# Gamma model makes more sense and has better AIC than regular LM

summary(lm1)
summary(lm2)
summary(lm2_grid)
summary(glm_gamma)


# prediction function ####
r_session = 500
baseline = (glm_gamma$coefficients[[1]])
b_nrow = (glm_gamma$coefficients[[2]])
b_n_layers = (glm_gamma$coefficients[[3]])
b_neurons = (glm_gamma$coefficients[[4]])



estimate_memory = function(nrow, n_layers, neurons) {
  
  baseline = 6.43  # (glm_gamma$coefficients[[1]])
  b_nrow = 2e-07  # (glm_gamma$coefficients[[2]])
  b_n_layers =  0.0053  # (glm_gamma$coefficients[[3]])
  b_neurons =  0.0030  # (glm_gamma$coefficients[[4]])
  
  memory = exp(baseline + (nrow * b_nrow) + (n_layers * b_n_layers) + (neurons * b_neurons)) # gamma model
  ceiling(memory * 1.3)  # scale by 30% to be save
}



