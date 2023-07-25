#' plot_volume_over_time
#'
#' @param data A tibble or data frame object
#' @param date_var The date variable ascribed to each observation
#' @param smooth To include a trend line utilizing geom_smooth?
#' @param vline_date Where to include a dashed line to mark a specific point on the x axis, if required
#' @param bar_colour What colour should the bar chart be? Provide a hexcode or accepted colour name
#' @param unit The unit of time the user wants to display when plotting, options are; "1 day", "1 week","2 weeks", "1 month", "1 year"
#' @param ... Input argument
#'
#' @return A bar chart showing basic volume over time across the data
#' @export
#'
#' @examples
#' plot_volume_over_time(data = data, date_var = date, smooth = FALSE, vline_date = "2023-04-01", bar_colour = colour_map, unit = "day")
plot_volume_over_time <- function (data,
                                   date_var = date_var,
                                   smooth = FALSE,
                                   vline_date = NULL,
                                   bar_colour = bar_colour,
                                   unit = c("day", "week", "month", "quarter", "year"),
                                   ...){
  time <- match.arg(unit)
  date_sym <- rlang::ensym(date_var)

  data <- data %>%
    dplyr::mutate(plot_date = lubridate::floor_date(!!date_sym, unit = time))

  plot <- data %>%
    dplyr::count(plot_date) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n)) +
    ggplot2::geom_col(fill = bar_colour) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_date(date_breaks = "1 months", date_labels = "%b-%y") +
    ggplot2::theme(plot.title = element_text(face = "bold"),
                   plot.title.position = "plot",
                   axis.text.x = element_text(angle = 0, vjust = 0.1),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.border = element_blank(),
                   axis.line = element_line(size = 0.5)) +
    ggplot2::labs(x = NULL, y = "# of posts", title = "Volume of Mentions over Time")

  if (smooth) {
    plot +  ggplot2::geom_smooth(colour = "#FFB600")
  }
    if(!is.null(vline_date)) {
      plot + ggplot2::geom_vline(xintercept = as.Date(vline_date), linetype = "dashed", size = 1, colour = "#6B695E")
    }
    else {
      return(plot)
    }
}

