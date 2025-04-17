#' Generate a MrIML co-occurrence network 
#'
#' This function generates a co-occurrence network from a provided list and
#' calculates strength and directionality of the relationships.
#'
#' @param mrBootstrap_obj A of bootstrapped partial dependencies output from
#' [mrBootstrap()].
#'
#' @return A data frame representing the co-occurrence network with strength and
#' directionality.
#'
#' @examplesIf !identical(Sys.getenv("NOT_CRAN"), "true")
#' library(tidymodels)
#' library(igraph)
#' library(ggnetwork)
#' 
#' data <- MRFcov::Bird.parasites
#' Y <- data %>%
#'   select(-scale.prop.zos) %>%
#'   dplyr::select(order(everything()))
#' X <- data %>%
#'   select(scale.prop.zos)
#'
#' model_rf <- rand_forest(
#'   trees = 100, # 100 trees are set for brevity. Aim to start with 1000
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
#'   k = 5
#' )
#' 
#' mrIML_rf_boot <- mrIML_rf %>%
#'   mrBootstrap()
#'
#' assoc_net_filtered <- mrIML_rf_boot %>% 
#'   mrCoOccurNet_bootstrap() %>%
#'   filter(mean_strength > 0.05)
#'  
#' #convert to igraph
#' g <- graph_from_data_frame(
#'   assoc_net_filtered,
#'   directed = TRUE,
#'   vertices = names(Y)
#' )
#' E(g)$Value <- assoc_net_filtered$mean_strength
#' E(g)$Color <- ifelse(
#'   assoc_net_filtered$direction == "negative",
#'   "blue", "red"
#' )
#' # Convert the igraph object to a ggplot object with NMDS layout
#' gg <- ggnetwork(g)
#' # Plot the graph
#' ggplot(
#'   gg,
#'   aes(x = x, y = y, xend = xend, yend = yend)
#' ) +
#'  geom_edges(
#'    aes(color = Color, linewidth = Value),
#'    curvature = 0.2,
#'    arrow = arrow(length = unit(5, "pt"), type = "closed")
#'  ) +
#'  geom_nodes(
#'    color = "gray",
#'    size = degree(g, mode = "out") / 2
#'  ) +
#'  scale_color_identity() +
#'  theme_void() +
#'  theme(legend.position = "none")  +
#'  geom_nodelabel_repel(
#'    aes(label = name),
#'    box.padding = unit(0.5, "lines"),
#'    size = 2,
#'    segment.colour = "black",
#'    colour = "white",
#'    fill = "grey36"
#'  )
#' @export

mrCoOccurNet_bootstrap <- function(mrBootstrap_obj){   #,  variable ='Plas
  # Expand bootstrap object
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
  # Filter to only taxa
  taxa <- pd_boot_df$response %>%
    unique()
  pd_boot_df <- pd_boot_df %>%
    dplyr::filter(var %in% taxa) %>%
    dplyr::mutate(X = ifelse(X == 1, "present", "absent"))
  # Calculate the strengths of edges
  edge_strength <- pd_boot_df %>%
    dplyr::group_by(var, response, bootstrap) %>%
    dplyr::summarise(sd_value = sd(value)) %>%
    dplyr::group_by(var, response) %>%
    dplyr::summarise(
      mean_strength = mean(sd_value),
      lower_ci = quantile(sd_value, probs = 0.025),
      upper_ci = quantile(sd_value, probs = 0.974)
    )
  # Calculate the direction of influence (positive or negative)
  edge_direction <- pd_boot_df %>%
    tidyr::spread(X, value) %>%
    dplyr::mutate(direction = present - absent) %>%
    dplyr::group_by(var, response) %>%
    dplyr::summarise(mean_direction = mean(direction)) %>%
    dplyr::mutate(direction = ifelse(mean_direction > 0, "positive", "negative"))
  
  # Join and return edge data for plotting
  dplyr::full_join(edge_strength, edge_direction) %>%
    dplyr::rename(taxa_1 = var, taxa_2= response) %>% 
    dplyr::select(
      taxa_1,
      taxa_2,
      direction,
      mean_strength,
      lower_ci,
      upper_ci,
      mean_direction
    )
  
}












