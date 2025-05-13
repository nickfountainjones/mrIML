#' Calculates and helps interpret variable importance for `mrIML` models.
#'
#' Summarizes variable importance in a `mrIML` model at both a global 
#' (across all the response models) and local (for individual response models) level. 
#' This can be done for a plain `mrIML` model or bootstrap results obtained from
#' [mrBootstrap()].
#'
#' @param mrIMLobj A list object output by [mrIMLpredicts()].
#' @param mrBootstrap_obj A list of bootstrap results output by [mrBootstrap()].
#' @param threshold The performance threshold for response models (AUC for
#' classification and R2 for regression). Only response models that meet this
#' performance criterion are plotted.
#' @param global_top_var The number of top global variables to display 
#' (default: 10).
#' @param local_top_var The number of top local variables for each response to
#' display (default: 5).
#' @param taxa A character string identifying which response model should be
#' plotted.
#' @param model_perf A list object containing model performance metrics output
#' by [mrIMLperformance()]. If not supplied, then [mrIMLperformance()] is run
#' inside `mrvip()` to get performance metrics.
#'
#' @return A list containing:
#' * `$vi_data`: Variable importance data in its raw form (including bootstrap
#' samples if `mrBootstrap_obj` was supplied).
#' * `$vi_tbl`: Variable importance data point estimates.
#' * `$vi_plot`: A grouped plot of the most important variables both globally and
#' for the individual response models.
#' 
#' @examples
#' library(tidymodels)
#' 
#' # Without bootstrap
#' data <- MRFcov::Bird.parasites
#' Y <- data %>%
#'   select(-scale.prop.zos) %>%
#'   select(order(everything()))
#' X <- data %>%
#'   select(scale.prop.zos)
#'
#' model_rf <- rand_forest(
#'   trees = 50, # 50 trees are set for brevity. Aim to start with 1000
#'   mode = "classification",
#'   mtry = tune(),
#'   min_n = tune()
#' ) %>%
#'   set_engine("randomForest")
#' 
#' mrIML_rf <- mrIMLpredicts(
#'   X = X,
#'   Y = Y,
#'   X1 = Y,
#'   Model = model_rf,
#'   prop = 0.7,
#'   k = 2,
#'   racing = FALSE
#' )
#' 
#' mrVip(mrIML_rf, taxa = "Plas")
#' 
#' # With bootstrap
#' 
#' mrIML_rf_boot <- mrIML_rf %>%
#'   mrBootstrap(num_bootstrap = 5)
#'   
#' mrIML_rf_vip <- mrVip(
#'   mrIML_rf,
#'   mrBootstrap_obj = mrIML_rf_boot
#' )
#' 
#' mrIML_rf_vip
#' 
#' @export
mrVip <- function(mrIMLobj,
                  mrBootstrap_obj = NULL,
                  threshold = 0.1,
                  global_top_var = 10,
                  local_top_var = 5,
                  taxa = NULL,
                  model_perf = NULL) {
  # Deconstruct mrIMLobj
  yhats <- mrIMLobj$Fits
  X <- mrIMLobj$Data$X
  Y <- mrIMLobj$Data$Y
  X1 <- mrIMLobj$Data$X1
  mode <- mrIMLobj$Model$mode
  
  # Get model performance if not supplied
  if (is.null(model_perf)) model_perf <- mrIMLperformance(mrIMLobj)
  
  # Reset global_ and local_top_var to the total number of variables if needed
  global_top_var <- ifelse(
    global_top_var > ncol(dplyr::bind_cols(X, X1)),
    ncol(dplyr::bind_cols(X, X1)),
    global_top_var
  )
  local_top_var <- ifelse(
    local_top_var > ncol(dplyr::bind_cols(X, X1)),
    ncol(dplyr::bind_cols(X, X1)),
    local_top_var
  )
  
  # Split workflow depending on if mrBootstrap_obj is supplied
  if (is.null(mrBootstrap_obj)) {
    vi_df <- mrVip_mrIMLobj(mrIMLobj)
  } else {
    vi_df <- mrVip_mrIMLboot(mrBootstrap_obj)
  }
  
  # Summarize in table form
  vi_tbl <- vi_df %>%
    dplyr::group_by(.data$response, .data$var) %>%
    dplyr::summarise(sd_value = mean(.data$sd_value)) %>%
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
    dplyr::group_by(.data$var) %>%
    dplyr::summarise(mean_imp = mean(.data$sd_value)) %>%
    dplyr::slice_max(order_by = .data$mean_imp, n = global_top_var)
  
  # Change geom type depending on if bootstrap object is supplied
  geom_VIplot <- ifelse(
    is.null(mrBootstrap_obj),
    ggplot2::geom_col,
    ggplot2::geom_boxplot
  )
  
  # Plot the variable importance scores
  p_global_vi <- vi_df %>%
    dplyr::filter(.data$var %in% highest_vi_df$var) %>%
    dplyr::group_by(.data$var) %>%
    dplyr::mutate(mean_imp = mean(.data$sd_value)) %>%
    dplyr::slice_max(
      order_by = .data$mean_imp,
      prop = (global_top_var / length(unique(vi_df$var)))
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$sd_value,
        y = stats::reorder(.data$var, .data$mean_imp)
      )
    ) +
    geom_VIplot() +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Importance", y = "Features") +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8)
    ) +
    ggplot2::xlim(0, NA)
  
  # Get only the fits that surpass user-supplied threshold
  perf_metric <- ifelse(mode == "classification", "roc_AUC", "rsquared")
  models_to_plot <- model_perf[[1]] %>%
    dplyr::rename(metric = .data[[perf_metric]]) %>%
    dplyr::filter(.data$metric > threshold) %>%
    dplyr::arrange(.data$metric) %>%
    utils::tail(9) %>%
    dplyr::pull(.data$response)
  
  # Plot the local response VIP in a grid
  p_local_vi_list <- lapply(
    models_to_plot,
    function(resp) {
      local_highest_vi_df <- vi_df %>%
        dplyr::filter(.data$response == resp) %>%
        dplyr::group_by(.data$var) %>%
        dplyr::summarise(mean_imp = mean(.data$sd_value)) %>%
        dplyr::slice_max(order_by = .data$mean_imp, n = local_top_var)
      
      vi_df %>%
        dplyr::filter(
          .data$response == resp,
          .data$var %in% local_highest_vi_df$var
        ) %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = .data$sd_value,
            y = stats::reorder(.data$var, .data$sd_value)
          )
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
  # Add user-defined taxa plot if needed
  #----------------------------------------------------------------------------- 
  
  if (!is.null(taxa)) {
    taxa_highest_vi_df <- vi_df %>%
      dplyr::filter(.data$response == taxa) %>%
      dplyr::group_by(.data$var) %>%
      dplyr::summarise(mean_imp = mean(.data$sd_value)) %>%
      dplyr::slice_max(order_by = .data$mean_imp, n = local_top_var)
    
    p_taxa_vi <- vi_df %>%
      dplyr::filter(
        .data$response == taxa,
        .data$var %in% taxa_highest_vi_df$var
      ) %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = .data$sd_value,
          y = stats::reorder(.data$var, .data$sd_value)
        )
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
  bootstrap_obj <- mrIMLboot_obj %>%
    unlist(recursive = FALSE)
  
  var_importance_df <- future.apply::future_lapply(
    bootstrap_obj,
    function(boot) {
      boot %>%
        dplyr::bind_rows(.id = "var") %>%
        dplyr::group_by(.data$var) %>%
        dplyr::summarise(
          sd_value = stats::sd(.data$value),
          response = unique(.data$response),
          bootstrap = unique(.data$bootstrap),
          .groups = "drop"
        )
    }
  ) %>%
    dplyr::bind_rows()
  
  var_importance_df
}

mrVip_mrIMLobj <- function(mr_iml_obj) {
  # Set flashlight metrics
  flashlight_ops <- mrIML_flashlight_setup(
    mr_iml_obj$Model$mode
  )
  
  # Run flashlight PD and summarize variable importance
  var_imp_list <- lapply(
    seq_along(mr_iml_obj$Fits),
    function(i) {
      fl <- flashlight::flashlight(
        model = mr_iml_obj$Fits[[i]]$last_mod_fit,
        label = names(mr_iml_obj$Fits)[i],
        y = 'class',
        data = mr_iml_obj$Fits[[i]]$data,
        predict_function = flashlight_ops$pred_fun,
        metrics = flashlight_ops$metrics
      )
      
      ligth_prof_df <- lapply(
        names(mr_iml_obj$Fits[[i]]$data)[-1],
        function(v_name) {
          pd <- flashlight::light_profile(fl, v_name)
          pd$data
        }
      ) %>%
        magrittr::set_names(names(mr_iml_obj$Fits[[i]]$data)[-1]) %>%
        dplyr::bind_rows(.id = "var") %>%
        dplyr::group_by(.data$var) %>%
        dplyr::summarise(
          sd_value = stats::sd(.data$value),
          response = names(mr_iml_obj$Fits)[i],
          bootstrap = NA,
          .groups = "drop"
        )
    }
  )
  
  dplyr::bind_rows(var_imp_list)
}

#' Principal Component Analysis of mrIML variable importance
#'
#' @param mrVip_obj A list returned by [mrVip()].
#'
#' @returns A list of PCA results:
#' * `$PCA_plot`: Side-by-side plots of the different response
#' models on the first two principal components (PCs) and a
#' Scree plot.
#' * `$PC_outliers`: A list of the models flagged as outliers
#' on at least one of the PCs.
#' * `$eigenvalues`: The eigenvalues associated with the
#' principal components.
#' * `$PC_scores`: The PC scores of each response model.
#' 
#' @examples
#' library(tidymodels)
#' 
#' # Without bootstrap
#' data <- MRFcov::Bird.parasites
#' Y <- data %>%
#'   select(-scale.prop.zos) %>%
#'   select(order(everything()))
#' X <- data %>%
#'   select(scale.prop.zos)
#'
#' model_rf <- rand_forest(
#'   trees = 50, # 50 trees are set for brevity. Aim to start with 1000
#'   mode = "classification",
#'   mtry = tune(),
#'   min_n = tune()
#' ) %>%
#'   set_engine("randomForest")
#' 
#' mrIML_rf <- mrIMLpredicts(
#'   X = X,
#'   Y = Y,
#'   X1 = Y,
#'   Model = model_rf,
#'   prop = 0.7,
#'   k = 2,
#'   racing = FALSE
#' )
#' 
#' mrIML_rf_vip <- mrVip(mrIML_rf, taxa = "Plas")
#' 
#' mrIML_rf_vip %>%
#'  mrVipPCA()
#' 
#' @export
mrVipPCA <- function(mrVip_obj) {
  
  # Require the 'ggrepel' package to be installed
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    message(
      paste0("The 'ggrepel' package is required for this function. Would you ",
             "like to install it? (yes/no)")
    )
    response <- readline()
    if (tolower(response) == "yes") {
      utils::install.packages("ggrepel")
    } else {
      stop(
        paste0("The 'ggrepel' package is needed for this function. Please ",
               "install it to proceed.")
      )
    }
  }
  
  vi_tbl <- mrVip_obj[[2]]
  
  pc_analasys <- vi_tbl %>%
    # Filter out rows with only one value
    dplyr::rowwise() %>%
    dplyr::filter(
      dplyr::n_distinct(dplyr::c_across(tidyselect::everything())) > 1
    ) %>%
    # Transpose
    t() %>%
    # PCA
    stats::prcomp(scale. = TRUE)
  
  # Get scores
  pc_scores <- pc_analasys$x %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      response = rownames(pc_analasys$x)
    )
  
  # Get eigenvalue info
  eigenvalues <- pc_analasys$sdev ^ 2
  var_explained <- tibble::tibble(
    PC = pc_analasys$x %>%
      colnames(),
    proportion =  (eigenvalues / sum(eigenvalues))
  )
  
  # Calculate outliers in each PC
  outliers <- apply(
    pc_scores %>%
      dplyr::select(-.data$response),
    MARGIN = 2,
    FUN = function(x) which((abs(x - stats::median(x)) / stats::mad(x)) > 6)
  )
  
  # Plot responses relative to first two PCs
  p_pc_plot <- pc_scores %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$PC1, y = .data$PC2, label = .data$response)
    ) +
    ggplot2::geom_point() +
    ggrepel::geom_label_repel() +
    ggplot2::theme_bw()
  
  # Plot the variability captured by each PC
  p_pc_var_explained <- var_explained %>%
    mutate(
      PC = factor(
        PC,
        levels = paste0("PC", 1:n()),
        ordered = TRUE
      )
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$PC, y = (.data$proportion * 100))
    ) +
    ggplot2::geom_col() +
    ggplot2::ylab("Variance explained (%)") +
    ggplot2::theme_bw() 
  
  # Return a list
  list(
    PCA_plot = patchwork::wrap_plots(p_pc_plot, p_pc_var_explained),
    PC_outliers = outliers,
    eigenvalues = eigenvalues,
    PC_scores = pc_scores
  )
}
