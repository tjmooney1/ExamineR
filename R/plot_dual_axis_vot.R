#' plot_dual_axis_vot
#' @description Plot two variables over time using two y axes
#' @param data A tibble or data frame object
#' @param date_var The date variable ascribed to each observation
#' @param variable_one The first variable the user wants to plot
#' @param variable_two The second variable, this one will be plotted on the second y axis
#'
#' @return A ggplot, with two y axes showing the trend of both variables over time
#' @export
#'
#' @examples
#' data %>% plot_dual_axis_vot(date_var = date, variable_one = cost, variable_two = impressions)
plot_dual_axis_vot <- function(data = data,
                               date_var = date_var,
                               variable_one = variable_one,
                               variable_two = variable_two,
                               custom_colours = custom_colours,
                               unit = c("1 year", "1 quarter", "1 month", "2 weeks", "1 week", "1 day")) {

  date_var <- dplyr::enexpr(date_var)
  variable_one <- dplyr::enexpr(variable_one)
  variable_two <- dplyr::enexpr(variable_two)

  # summarise the data
  summarised_data <- data %>%
    dplyr::mutate(date = base::as.Date(!!date_var)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(mean_var1 = base::mean(!!variable_one, na.rm = TRUE),
                     mean_var2 = base::mean(!!variable_two, na.rm = TRUE))

  # calculates the scaling coefficient
  coeff <- max(max(summarised_data$mean_var1), max(summarised_data$mean_var2)) / min(max(summarised_data$mean_var1), max(summarised_data$mean_var2))

  # ensure to assign undefined variable names
  variable_one_name <- deparse(substitute(variable_one))
  variable_two_name <- deparse(substitute(variable_two))

  plot <- ggplot2::ggplot(summarised_data, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = mean_var1, color = variable_one_name)) +
    ggplot2::geom_line(ggplot2::aes(y = mean_var2 / coeff, color = variable_two_name)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Dual Axes Time Series Plot", x = "Date", color = "") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, vjust = 1),
                   legend.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   text = ggplot2::element_text(color = "#000000", family = "Helvetica", size = 12),
                   plot.title = ggplot2::element_text(face = "bold", size = 16, margin = ggplot2::margin(b = 10)),
                   plot.subtitle = ggplot2::element_text(size = 14, margin = ggplot2::margin(b = 10))) +
    ggplot2::scale_y_continuous(name = variable_one_name, sec.axis = ggplot2::sec_axis(~.*coeff, name = variable_two_name)) +
    ggplot2::scale_x_date(date_breaks = unit) +
    ggplot2::scale_color_manual(values = custom_colours)

  return(plot)
}

