#' Bootstrap Partial Dependence plots
#'
#' This function bootstraps model predictions and generates partial dependence plots for each response variable.
#' It also creates a combined plot for the top variables of interest.
#'
#' @param mrBootstrap_obj A list of model bootstraps generated using mrBootstrap function.
#' @param vi_obj Variable Importance data.
#' @param X The predictor data.
#' @param Y The response data.
#' @param target The target variable for generating plots.
#' @param global_top_var The number of top variables to consider (default: 2).
#' 
#' @return A list containing the partial dependence plots for each response variable and a combined plot.
#' @export
#'
#' @examples
#' \dontrun{
#'#' # Example usage:
#' #set up analysis
#' Y <- dplyr::select(Bird.parasites, -scale.prop.zos)%>% 
#' dplyr::select(sort(names(.)))#response variables eg. SNPs, pathogens, species....
#' X <- dplyr::select(Bird.parasites, scale.prop.zos) # feature set

#' X1 <- Y %>%
#' dplyr::select(sort(names(.)))
#'model_rf <- 
#' rand_forest(trees = 100, mode = "classification", mtry = tune(), min_n = tune()) %>% #100 trees are set for brevity. Aim to start with 1000
#' set_engine("randomForest")
#' yhats_rf <- mrIMLpredicts(X=X, Y=Y,
#'X1=X1,'Model=model_rf , 
#'balance_data='no',mode='classification',
#'tune_grid_size=5,seed = sample.int(1e8, 1),'morans=F,
#'prop=0.7, k=5, racing=T) #
#'bs_analysis <- mrBootstrap(yhats=yhats_rf,Y=Y, num_bootstrap = 5)
#'pds <- mrPD_bootstrap(mrBootstrap_obj=bs_malaria, vi_obj=bs_impVIa, X, Y,
#'target='Plas', global_top_var=5)
#'pd_list <- pds[[1]] #data
#'pds[[2]]#plot }


mrPD_bootstrap <- function(mrBootstrap_obj,
                           vi_obj,
                           X, Y,
                           target,
                           global_top_var = 2) {
  
  n_response <- ncol(Y)
  complete_df <- cbind(Y, X)
  n_data <- ncol(complete_df)
  
  # Colapse bootstrap results into a dataframe
  pd_boot_df <- lapply(
    mrBootstrap_obj %>%
      purrr::flatten() %>%
      purrr::flatten(),
    function(pd_df) {
      names(pd_df)[1] <- "X"
      pd_df
    }
  ) %>%
  dplyr::bind_rows(.id = "var")
  # Filter to target (do this separately because need to output full df)
  pd_boot_df_target <- pd_boot_df %>%
    dplyr::filter(response == target)
  # Get list of vars to plot according to VI
  vi_obj <- vi_obj[[1]]
  important_vars <- vi_obj %>%
    dplyr::filter(response == target) %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(mean_sd = mean(sd_value)) %>%
    dplyr::arrange(dplyr::desc(mean_sd)) %>%
    head(global_top_var) %>%
    dplyr::pull(var)
  # Create PD plots
  plot_list <- lapply(
    important_vars,
    function(v) {
      pd_var_df <- pd_boot_df_target %>%
        dplyr::filter(var == v)
      if (length(unique(pd_var_df$X)) == 2) {
        plot_disc_pd(pd_var_df, v, target)
      } else {
        plot_cont_pd(pd_var_df, v, target)
      }
    }
  )
  # Plot in grid
  list(
    pd_boot_df,
    patchwork::wrap_plots(plot_list)
  )
}

plot_cont_pd <- function(pd_var_df, var_name, resp_name) {
  pd_var_df %>%
    dplyr::group_by(bootstrap) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = X, y = value, group = bootstrap)
    ) +
    ggplot2::geom_line(alpha = 0.3) +
    ggplot2::labs(x = var_name, y = paste(resp_name, "prob")) +
    ggplot2::theme_bw()
}
plot_disc_pd <- function(pd_var_df, var_name, resp_name) {
  pd_var_df %>%
    ggplot2::ggplot(
      ggplot2::aes(x = ifelse(X == 1, "present", "absent"), y = value)
    ) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = var_name, y = paste(resp_name, "prob")) +
    ggplot2::theme_bw() 
}
