#' plot_sentiment_over_time
#'
#' @param data A tibble or data frame object
#' @param date_var The date variable ascribed to each observation
#' @param sentiment_var The sentiment variable showing the observations ascribed sentiment based on classification
#' @param sentiment_colors For if the user has a custom colour scale or map they wish to assign each sentiment
#' @param vline_date Where to include a dashed line to mark a specific point on the x axis, if required
#' @param unit The unit of time the user wants to display when plotting, options are; "1 day", "1 week","2 weeks", "1 month", "1 year"
#' @param ... Input argument
#'
#' @return A stacked bar chart that maps sentiment distribution per day OR week OR month OR year over time
#' @export
#'
#' @examples
#' plot_sentiment_over_time(data = data, date_var = date, sentiment_var = sentiment, sentiment_colors = colour_map, vline_date = "2023-04-01", unit = "day")
plot_sentiment_over_time <- function (data = data,
                                      date_var = date_var,
                                      sentiment_var = sentiment,
                                      sentiment_colors = sentiment_colors,
                                      vline_date = NULL,
                                      unit = c("day", "week", "month", "quarter", "year"),
                                      ...){
  time <- match.arg(unit)
  date_sym <- rlang::ensym(date_var)
  sentiment_sym <- rlang::ensym(sentiment_var)

  data <- data %>%
    dplyr::mutate(plot_date = lubridate::floor_date(!!date_sym, unit = time))

  plot <- data %>%
    dplyr::count(plot_date, !!sentiment_sym) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n, fill = !!sentiment_sym)) +
    ggplot2::geom_col() +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") +
    ggplot2::theme(plot.title = element_text(face = "bold"),
                   plot.title.position = "plot",
                   axis.text.x = element_text(angle = 0, vjust = 0.1),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.border = element_blank(),
                   axis.line = element_line(size = 0.5)) +
    ggplot2::labs(x = NULL, y = "# of posts", title = "Sentiment Distribution over Time")

  if (!is.null(vline_date)) {
    plot <- plot + ggplot2::geom_vline(xintercept = as.Date(vline_date), linetype = "dashed", size = 1, colour = "#6B695E")
  }

  # Custom colors for sentiment categories
  plot <- plot + ggplot2::scale_fill_manual(values = sentiment_colors)

  return(plot)
}
