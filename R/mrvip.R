#' Calculates and helps interpret variable importance for [mrIML] models.
#'
#' Summarizes variable importance in a [mrIML] model at both a global (accross
#' all the response models) and local (for individual response models). This can
#' be done for a plain [mrIML] model or bootstrap results obtained from
#' [mrBootstrap()]. 
#'
#' @param mrIMLobj A list object output by [mrIMLpredict()].
#' @param mrBootstrap_obj A list of bootstrap results output by [mrBootstrap()].
#' @param ModelPerf A list object containing model performance metrics output by
#' [mrIMLperformance].
#' @param threshold The performance threshold for response models (AUC for
#' classification and $R^2$ for regression). Only response models that meet this
#' performance criteria are plotted.
#' @param global_top_var The number of top global variables to display
#' (default: 10).
#' @param local_top_var The number of top local variables for each response to
#' display (default: 5).
#' 
#' @return A list with:
#' * `$vi_data`: Variable importance data its raw form (including bootstrap
#' samples if `mrBootstrap_obj` was supplied).
#' * `$vi_tbl`: Variable importance data point estimates.
#' * `$vi_plot`: A group plot of the most important variables both globally and
#' for the individual response models.
#' 
#' @examples
#' # example code
#' 
#' @export

mrvip <- function(mrIMLobj,
                  mrBootstrap_obj = NULL,
                  threshold = 0.1,
                  global_top_var = 10,
                  local_top_var = 5,
                  taxa = NULL,
                  model_perf = NULL,
                  plot_pca = TRUE) {
  # Deconstruct mrIMLobj
  yhats <- mrIMLobj$Fits
  X <- mrIMLobj$Data$X
  Y <- mrIMLobj$Data$Y
  X1 <- mrIMLobj$Data$X1
  mode <- mrIMLobj$Model$mode
  # Reset global_ and local_top_var to the total number of variables if needed
  global_top_var <- ifelse(
    global_top_var > ncol(cbind(X, X1)),
    ncol(cbind(X, X1)),
    global_top_var
  )
  local_top_var <- ifelse(
    local_top_var > ncol(cbind(X, X1)),
    ncol(cbind(X, X1)),
    local_top_var
  )
  # Set local options to be restored on exit
  #withr::local_options(list(dplyr.summarise.inform = FALSE))
  
  # Split workflow depending on if mrBootstrap_obj is supplied
  # (later this could be an S3 method)
  if (is.null(mrBootstrap_obj)) {
    vi_df <- mrVip_mrIMLobj(yhats)
  } else {
    vi_df <- mrVip_mrIMLboot(mrBootstrap_obj)
  }
  
  # Summarize in table form(used for mrVipPCA)
  vi_tbl <- vi_df %>%
    dplyr::group_by(response, var) %>%
    dplyr::summarise(sd_value = mean(sd_value)) %>%
    tidyr::pivot_wider(
      id_cols = "var",
      names_from = "response",
      values_from = "sd_value"
    ) %>%
    tibble::column_to_rownames("var") %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::everything(),
        ~replace(., is.na(.), mean(., na.rm = TRUE))
      )
    )
  
  #-----------------------------------------------------------------------------
  # General plotting
  #-----------------------------------------------------------------------------
  # Get top global variables
  highest_vi_df <- vi_df %>% 
    dplyr::group_by(var) %>% 
    dplyr::summarise(mean_imp = mean(sd_value)) %>%
    dplyr::slice_max(
      order_by = mean_imp,
      n = global_top_var
    )
  # Change geom type depending on if bootstrap object is supplied or not
  geom_VIplot <- ifelse(
    is.null(mrBootstrap_obj),
    ggplot2::geom_col, # Is this right?????
    ggplot2::geom_boxplot
  )
  # Plot the variable importance scores
  p_global_vi <- vi_df %>% 
    dplyr::filter(var %in% highest_vi_df$var) %>%
    dplyr::group_by(var) %>% 
    dplyr::mutate(mean_imp = mean(sd_value)) %>%
    dplyr::slice_max(
      order_by = mean_imp,
      prop = (global_top_var / length(unique(vi_df$var)))
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = sd_value, y = reorder(var, mean_imp))
    ) +
    geom_VIplot() +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Importance", y = "Features") +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8)
    ) +
    ggplot2::xlim(0, NA)
  
  # Get only the fits that surpass user suplied threashold
  perf_metric <- ifelse(mode == "classification", "roc_AUC", "rsquared")
  models_to_plot <- model_perf[[1]] %>%
    dplyr::rename(metric = perf_metric) %>%
    # Filter to only fits that meet theshold criteria
    dplyr::filter(
      metric > threshold
    ) %>%
    dplyr::arrange(metric, ) %>%
    # Limit to the top 9 for uncrowded plotting 
    tail(9) %>%
    dplyr::pull(response)
  
  # Plot the local response VIP in a grid
  p_local_vi_list <- lapply(
    models_to_plot,
    function(resp) {
      local_highest_vi_df <- vi_df %>%
        dplyr::filter(response == resp) %>%
        dplyr::group_by(var) %>% 
        dplyr::summarise(mean_imp = mean(sd_value)) %>%
        dplyr::slice_max(
          order_by = mean_imp,
          n = local_top_var
        )
      vi_df %>%
        dplyr::filter(
          response == resp,
          var %in% local_highest_vi_df$var
        ) %>%
        dplyr::group_by() %>%
        ggplot2::ggplot(
          ggplot2::aes(x = sd_value, y = reorder(var, sd_value))
        ) +
        ggplot2::geom_boxplot() +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Importance", y = "") +
        ggplot2::scale_y_discrete(label = abbreviate) +
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(size = 8),
          axis.title.y = ggplot2::element_text(size = 8)
        ) +
        ggplot2::labs(subtitle = resp) +
        ggplot2::xlim(0, max(vi_df$sd_value))
    }
  )
  
  p_local_vi <- patchwork::wrap_plots(p_local_vi_list, axis_titles = "collect")
  
  #-----------------------------------------------------------------------------
  # Add user defined taxa plot if needed
  #----------------------------------------------------------------------------- 
  
  if(!is.null(taxa)) {
    taxa_highest_vi_df <- vi_df %>%
      dplyr::filter(response == taxa) %>%
      dplyr::group_by(var) %>% 
      dplyr::summarise(mean_imp = mean(sd_value)) %>%
      dplyr::slice_max(
        order_by = mean_imp,
        n = local_top_var
      )
    p_taxa_vi <- vi_df %>%
      dplyr::filter(
        response == taxa,
        var %in% taxa_highest_vi_df$var
      ) %>%
      dplyr::group_by() %>%
      ggplot2::ggplot(
        ggplot2::aes(x = sd_value, y = reorder(var, sd_value))
      ) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Importance", y = "Features") +
      ggplot2::scale_y_discrete(label = abbreviate) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(size = 8),
        axis.title.y = ggplot2::element_text(size = 8)
      ) +
      ggplot2::labs(subtitle = taxa) +
      ggplot2::xlim(0, max(vi_df$sd_value))
    
    p_VIP <- (p_global_vi + p_local_vi) / p_taxa_vi
      
  } else {
    p_VIP <- (p_global_vi + p_local_vi)
  }
  
  #-----------------------------------------------------------------------------
  # Return a list
  #-----------------------------------------------------------------------------
  
  list(
    vi_df,
    vi_tbl,
    p_VIP
  )
  
}

mrVip_mrIMLboot <- function(mrIMLboot_obj) {
  # Unlist the object at the top level
  bootstrap_obj <- bs_malaria %>% # << change bs_malaria!
    unlist(recursive = FALSE)
  
  var_importance_df <- future.apply::future_lapply(
    bootstrap_obj,
    function(boot) {
      boot %>%
        dplyr::bind_rows(.id = "var") %>%
        dplyr::group_by(var) %>%
        dplyr::summarise(
          sd_value = sd(value),
          response = unique(response),
          bootstrap = unique(bootstrap)
        )
    }
  ) %>%
    dplyr::bind_rows()
  
  # Should be set up now to generate plots from large global df...
  var_importance_df
}

mrVip_mrIMLobj <- function(yhats) {
  # Run flashlight on mrIML object and then get sd of partial dependence values
  ## Set flashlight metrics
  flashlight_ops <- mrIML_flashlight_setup(
    mode
  )
  ## Run flashlight PD and summarize variable importance via SD.
  var_imp_list <- lapply(
    seq_along(yhats),
    function(i) {
      fl <- flashlight::flashlight(
        model = yhats[[i]]$last_mod_fit,
        label = names(yhats)[i],
        y = 'class', 
        data = yhats[[i]]$data,
        predict_function = flashlight_ops$pred_fun,
        metrics = flashlight_ops$metrics
      )
      ligth_prof_df <- lapply(
        names(yhats[[i]]$data)[-1],
        function(v_name) {
          pd <- flashlight::light_profile(
            fl,
            v_name
          )
          pd$data
        }
      ) %>%
        magrittr::set_names(names(yhats[[i]]$data)[-1]) %>%
        dplyr::bind_rows(.id = "var") %>%
        dplyr::group_by(var) %>%
        dplyr::summarise(
          sd_value = sd(value),
          response = names(yhats)[i],
          bootstrap = NA
        )
    }
  )
  # Return a df to plot
  ## Bind flashlight PD results
  var_imp_list %>%
    dplyr::bind_rows()
}

#' Principle Component Analysis of [mrIML] variable importance
#' 
#' @param mrVip_obj A list returned by [mrVip()].
#' 
#' @returns A list of PCA results:
#' * `$PCA_plot`
#' * `$PC_outlires`
#' * `$eigenvalues`
#' * `$PC_scores`
#' 
#' @export

mrVipPCA <- function(mrVip_obj) {
  vi_tbl <- mrVip_obj[[2]]
  # Run PCA
  pc_analasys <- t(vi_tbl) %>%
    prcomp()
  # Get scores
  pc_scores <- pc_analasys$x %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      response = rownames(pc_analasys$x)
    )
  # Get eigenvalue info
  eigenvalues <- pc_analasys %>%
    generics::tidy(matrix = "eigenvalues")
  # Calculate outliers in each PC
  outliers <- apply(
    pc_scores %>%
      dplyr::select(-response),
    MARGIN = 2,
    FUN = function(x) which((abs(x - median(x)) / mad(x)) > 6)
  )
  # Plot responses relative to first two PCs
  p_pc_plot <- pc_scores %>%
    ggplot2::ggplot(
      ggplot2::aes(x = PC1, y = PC2, label = response)
    ) +
    ggplot2::geom_point() +
    ggrepel::geom_label_repel() +
    ggplot2::theme_bw()
  # Plot the variability captured by each PC
  p_pc_var_explained <- eigenvalues %>%
    ggplot2::ggplot(
      ggplot2::aes(x = PC, y = percent)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(breaks = 1:9) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(),
      expand = ggplot2::expansion(mult = c(0, 0.01))
    ) +
    ggplot2::theme_bw() 
  # Return a list
  list(
    PCA_plot = patchwork::wrap_plots(p_pc_plot, p_pc_var_explained),
    PC_outliers = outliers,
    eigenvalues = eigenvalues,
    PC_scores = pc_scores
  )
}
