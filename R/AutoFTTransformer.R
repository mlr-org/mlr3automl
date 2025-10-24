#' @title FTTransformer Auto
#'
#' @include mlr_auto.R
#'
#' @description
#' FTTransformer auto.
#'
#' @template param_id
#' @template param_task
#' @template param_measure
#' @template param_n_threads
#' @template param_timeout
#' @template param_devices
#'
#' @export
AutoFTTransformer = R6Class("AutoFTTransformer",
  inherit = Auto,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "ft_transformer") {
      super$initialize(
        id = id,
        properties = "internal_tuning",
        task_types = c("classif", "regr"),
        packages = c("mlr3", "mlr3torch"),
        devices = "cuda"
      )
    },

    #' @description
    #' Create the graph for the auto.
    graph = function(task, measure, n_threads, timeout, devices) {
      assert_task(task)
      assert_measure(measure)
      assert_count(n_threads)
      assert_count(timeout)
      assert_subset(devices, c("cuda", "cpu"))

      require_namespaces("mlr3torch")

      device =  if ("cuda" %in% devices) "cuda" else "auto"

      # copied from mlr3tuningspaces
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

      learner = lrn(sprintf("%s.ft_transformer", task$task_type),
       id = "ft_transformer",
       measures_valid = measure,
       patience = self$early_stopping_rounds(task),
       batch_size = 32L,
       attention_n_heads = 8L,
       opt.param_groups = rtdl_param_groups,
       device = device
      )
      set_threads(learner, n_threads)

      po("removeconstants", id = "ft_transformer_removeconstants") %>>%
        po("imputeoor", id = "ft_transformer_imputeoor") %>>%
        po("fixfactors", id = "ft_transformer_fixfactors") %>>%
        po("imputesample", affect_columns = selector_type(c("factor", "ordered")), id = "ft_transformer_imputesample") %>>%
        po("encodeimpact", id = "ft_transformer_encode") %>>%
        po("scale", id = "ft_transformer_scaler") %>>%
        po("removeconstants", id = "ft_transformer_post_removeconstants") %>>%
        learner
    },

    #' @description
    #' Estimate the memory for the auto.
    estimate_memory = function(task) {
      memory_size = task$nrow * task$ncol * 8 * 10 / 1e6
      lg$info("FTTransformer memory size: %s MB", round(memory_size))
      ceiling(memory_size)
    }
  ),

  private = list(
    .search_space = ps(
        ft_transformer.n_blocks                = p_int(1, 6),
        ft_transformer.d_token                 = p_int(8L, 64L, trafo = function(x) 8L * x),
        ft_transformer.residual_dropout        = p_dbl(0, 0.2),
        ft_transformer.attention_dropout       = p_dbl(0, 0.5),
        ft_transformer.ffn_dropout             = p_dbl(0, 0.5),
        ft_transformer.ffn_d_hidden_multiplier = p_dbl(2 / 3, 8 / 3),
        ft_transformer.opt.lr                  = p_dbl(1e-5, 1e-4, logscale = TRUE),
        ft_transformer.opt.weight_decay        = p_dbl(1e-6, 1e-3, logscale = TRUE),
        ft_transformer.epochs                  = p_int(1L, 100L, tags = "internal_tuning", aggr = function(x) as.integer(ceiling(mean(unlist(x)))))
    ),

    .default_values = list(
      ft_transformer.n_blocks = 4L,
      ft_transformer.d_token = 64L,
      ft_transformer.residual_dropout = 0.1,
      ft_transformer.attention_dropout = 0.2,
      ft_transformer.ffn_dropout = 0.2,
      ft_transformer.ffn_d_hidden_multiplier = 2 / 3,
      ft_transformer.opt.lr = 1e-5,
      ft_transformer.opt.weight_decay = 1e-6
    )
  )
)

mlr_auto$add("ft_transformer", function() AutoFTTransformer$new())
