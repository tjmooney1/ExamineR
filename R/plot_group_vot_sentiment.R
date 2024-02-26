#' plot_group_vot_sentiment
#' @description Plots a faceted graphic, showing volume over time as well as sentiment per group
#' @param data A tibble or data frame object
#' @param sentiment_var The sentiment variable showing the observations ascribed sentiment based on classification
#' @param date_var The date variable ascribed to each observation
#' @param group_var The group variable. For example, topic or audience
#' @param unit The unit of time the user wants to display when plotting, options are; "day", "week", "month", "quarter", "year"
#'
#' @return A faceted plot displaying volume and sentiment over time per group
#' @export
#'
#' @examples
#' topics_vot_sentiment_day <- plot_group_vot_sentiment(daat = data, sentiment_var = sentiment, date_var = date, group_var = topic, unit = "day")
#' plot_group_vot_sentiment(data = data sentiment_var = sentiment, date_var = date, group_var = topic, unit = "day")
plot_group_vot_sentiment <- function(data = data,
                                     sentiment_var = sentiment,
                                     date_var = date,
                                     group_var = group,
                                     unit = c("day", "week", "month", "quarter", "year")){

  unit <- match.arg(unit)
  sent_sym <- rlang::ensym(sentiment_var)
  date_sym <- rlang::ensym(date_var)
  group_sym <- rlang::ensym(group_var)

  sent_string <- rlang::as_string(sent_sym)
  date_string <- rlang::as_string(date_sym)

  if(!sent_string %in% colnames(data)){
    stop(paste0("Cannot find '", sent_string, "' in the data frame, did you mean `sentiment_var = sentiment`?"))
  }
  if(!date_string %in% colnames(data)){
    stop(paste0("Cannot find '", date_string, "' in the data frame, did you mean `date_var = date`?"))
  }

  data <- data %>% dplyr:: mutate(
    plot_date = as.Date(!!date_sym),
    plot_date = lubridate::floor_date(plot_date, unit = unit),
    facet_var = !!group_sym)

  plot <- data %>%
    dplyr::count(plot_date,!!sent_sym, facet_var) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n, colour = !!sent_sym)) +
    ggplot2::geom_col(fill = "grey", colour = "grey") +
    ggplot2::geom_line(size = 0.75) +
    ggplot2::scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b") +
    ggplot2::scale_fill_manual(aesthetics = c("fill", "colour"),
                               values = c("NEGATIVE" = "#8b0000",
                                          "NEUTRAL" = "orange",
                                          "POSITIVE" = "#008b00")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45)) +
    ggplot2::labs(y = "n", x = paste0("Date by ", unit)) +
    ggplot2::facet_wrap(~facet_var, nrow = 3)

  return(plot)
}
