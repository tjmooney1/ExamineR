#' plot_group_vot
#' @description Plots a grouo variables volume over time(vot). There are two options for the user; facet or list.
#' @param data A tibble or data frame object
#' @param group_var The group variable. For example, topic or audience
#' @param date_var The date variable ascribed to each observation
#' @param unit The unit of time the user wants to display when plotting, options are; "day", "week", "month", "quarter", "year"
#' @param nrow The number of rows that the group variables will plotted on for when the user selects plot_type = "facet", is not required when calling "list"
#' @param plot_type The user has an option to select a facet plot or to plot each group variable vot individually, a list of plots. This argument is supported by "facet" or "list"
#' @param colour_mapping For if there is an alternative colour scale that could be used to colour the group variable
#'
#' @return A ggplot showing each group variables volume of time, either as a facetted plot or a list of plots
#' @export
#'
#' @examples
#' plot_group_vot(data = data, group_var = group, date_var = date, unit = "day", nrow = 5, plot_type = "facet", colour_mapping = colour_map)
#' plot_list <- plot_group_vot(data = data, group_var = group, date_var = date, unit = "day", plot_type = "list", colour_mapping = colour_map)
plot_group_vot <- function(data,
                           group_var,
                           date_var,
                           unit = c("day", "week", "month", "quarter", "year"),
                           nrow = 2,
                           plot_type = c("facet", "list"),
                           colour_mapping = NULL) {

  # Validate inputs
  unit <- match.arg(unit)
  plot_type <- match.arg(plot_type)

  # Prepare symbols
  date_sym <- rlang::ensym(date_var)
  group_sym <- rlang::ensym(group_var)

  # Common data manipulation
  data <- data %>%
    dplyr::mutate(plot_date = lubridate::floor_date(!!date_sym, unit = unit),
                  facet_var = !!group_sym)

  if (plot_type == "facet") {
    # Calculate total count of each group variable and order by count
    group_counts <- data %>%
      dplyr::count(facet_var) %>%
      dplyr::arrange(desc(n))

    # Create facet plot with ordered group variable
    plot <- data %>%
      dplyr::mutate(facet_var = factor(facet_var, levels = group_counts$facet_var)) %>%
      dplyr::count(plot_date, facet_var) %>%
      ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n, fill = facet_var)) +
      ggplot2::geom_col() +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_date(date_breaks = "1 months", date_labels = "%d-%b") +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(size = 7.5, angle = 45, hjust = 0.9),
                     axis.ticks.x.bottom = ggplot2::element_line(size = 0.3),
                     panel.grid = ggplot2::element_blank(),
                     panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 0.3)) +
      ggplot2::labs(title = "", x = NULL, y = "Number of Posts") +
      ggplot2::scale_fill_manual(values = colour_mapping) +
      ggplot2::facet_wrap(~facet_var, nrow = nrow)

    print(plot)
  } else if (plot_type == "list") {
    # Create list of plots
    unique_groups <- data %>%
      dplyr::pull(!!group_sym) %>%
      unique()

    plots <- list()
    for (group in unique_groups) {
      plot_data <- data %>%
        dplyr::filter(!!group_sym == group) %>%
        dplyr::count(plot_date)

      if (is.null(colour_mapping)) {
        plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = plot_date, y = n)) +
          ggplot2::geom_col() +
          ggplot2::theme_minimal() +
          ggplot2::scale_x_date(date_breaks = "1 months", date_labels = "%d-%b") +
          ggplot2::theme(legend.position = "none",
                         axis.text.x = ggplot2::element_text(angle = 45),
                         panel.grid = ggplot2::element_blank()) +
          ggplot2::labs(title = paste("Topic Volume over Time -", group), x = NULL, y = "Number of Posts")
      } else {
        plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = plot_date, y = n, fill = factor(group))) +
          ggplot2::geom_col(fill = colour_mapping[group]) +
          ggplot2::theme_minimal() +
          ggplot2::scale_x_date(date_breaks = "1 months", date_labels = "%d-%b") +
          ggplot2::theme(legend.position = "none",
                         axis.text.x = ggplot2::element_text(angle = 45),
                         panel.grid = ggplot2::element_blank()) +
          ggplot2::labs(title = paste("Topic Volume over Time -", group), x = NULL, y = "Number of Posts")
      }

      plots[[group]] <- plot
    }
    return(plots)
  } else {
    stop("Invalid plot_type argument. Try 'facet' or 'list'.")
  }
}
