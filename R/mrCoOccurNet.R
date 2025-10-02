#' Generate a MrIML co-occurrence network
#'
#' This function generates a co-occurrence network from a provided list and
#' calculates strength and directionality of the relationships. The output can be
#' passed to \pkg{igraph} to plot a directed acyclic graph (DAG).
#'
#' @param mrBootstrap_obj A list of bootstrapped partial dependencies output from
#' [mrBootstrap()].
#'
#' @return A data frame representing the co-occurrence network with edge
#' strengths and directionality.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' library(tidymodels)
#' library(igraph)
#' library(ggnetwork)
#'
#' mrIML_rf <- mrIML::mrIML_bird_parasites_RF
#'
#' mrIML_rf_boot <- mrIML_rf %>%
#'   mrBootstrap()
#'
#' assoc_net_filtered <- mrIML_rf_boot %>%
#'   mrCoOccurNet() %>%
#'   filter(mean_strength > 0.05)
#'
#' # Convert to igraph
#' g <- graph_from_data_frame(
#'   assoc_net_filtered,
#'   directed = TRUE,
#'   vertices = names(mrIML_rf$Data$Y)
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
#'   geom_edges(
#'     aes(color = Color, linewidth = Value),
#'     curvature = 0.2,
#'     arrow = arrow(length = unit(5, "pt"), type = "closed")
#'   ) +
#'   geom_nodes(
#'     color = "gray",
#'     size = degree(g, mode = "out") / 2
#'   ) +
#'   scale_color_identity() +
#'   theme_void() +
#'   theme(legend.position = "none") +
#'   geom_nodelabel_repel(
#'     aes(label = name),
#'     box.padding = unit(0.5, "lines"),
#'     size = 2,
#'     segment.colour = "black",
#'     colour = "white",
#'     fill = "grey36"
#'   )
#'
#' @export
mrCoOccurNet <- function(mrBootstrap_obj) {
  stopifnot(
    "mrCoOccurNet() only available for classification models." = attr(
      mrBootstrap_obj,
      "mode"
    ) ==
      "classification"
  )
  # Expand bootstrap object
  pd_boot_df <- lapply(
    mrBootstrap_obj %>%
      purrr::flatten() %>%
      purrr::flatten(),
    function(pd_df) {
      names(pd_df)[1] <- "X"
      pd_df %>%
        dplyr::mutate(
          X = as.numeric(.data$X)
        )
    }
  ) %>%
    dplyr::bind_rows(.id = "var")

  # Filter to only taxa
  taxa <- pd_boot_df %>%
    dplyr::pull(.data$response) %>%
    unique()

  pd_boot_df <- pd_boot_df %>%
    dplyr::filter(.data$var %in% taxa) %>%
    dplyr::mutate(X = ifelse(.data$X == 1, "present", "absent"))

  # Calculate the strengths of edges
  edge_strength <- pd_boot_df %>%
    dplyr::group_by(.data$var, .data$response, .data$bootstrap) %>%
    dplyr::summarise(sd_value = stats::sd(.data$value), .groups = "drop") %>%
    dplyr::group_by(.data$var, .data$response) %>%
    dplyr::summarise(
      mean_strength = mean(.data$sd_value, na.rm = TRUE),
      lower_ci = stats::quantile(.data$sd_value, probs = 0.025, na.rm = TRUE),
      upper_ci = stats::quantile(.data$sd_value, probs = 0.974, na.rm = TRUE),
      .groups = "drop"
    )

  # Calculate the direction of influence (positive or negative)
  edge_direction <- pd_boot_df %>%
    tidyr::pivot_wider(names_from = .data$X, values_from = .data$value) %>%
    dplyr::mutate(direction = .data$present - .data$absent) %>%
    dplyr::group_by(.data$var, .data$response) %>%
    dplyr::summarise(
      mean_direction = mean(.data$direction),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      direction = ifelse(.data$mean_direction > 0, "positive", "negative")
    )

  # Join and return edge data for plotting
  dplyr::full_join(
    edge_strength,
    edge_direction,
    by = c("var", "response")
  ) %>%
    dplyr::rename(taxa_1 = .data$var, taxa_2 = .data$response) %>%
    dplyr::select(
      .data$taxa_1,
      .data$taxa_2,
      .data$direction,
      .data$mean_strength,
      .data$lower_ci,
      .data$upper_ci,
      .data$mean_direction
    )
}
