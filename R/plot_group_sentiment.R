#' plot_group_sentiment
#' @description Plots a stacked bar chart showing the sentiment distribution of a grouped variable. A volume count is also included, showing how many observations per group make up the sentiment distribution
#' @param data A tibble or data frame object
#' @param group_var The group variable. For example, topic or audience
#' @param sentiment_var The sentiment variable showing the observations ascribed sentiment based on classification
#' @param colours For if the user wishes to amend the existing colour scale
#'
#' @return A simple bar chart showing how any group variables' sentiment is distributed
#' @export
#'
#' @examples
#' topics_sentiment_distribution <- plot_group_sentiment(data = data, group_var = topic, sentiment_var = sentiment, colours = sentiment_colours)
#' plot_group_sentiment(data = data, group_var = topic, sentiment_var = sentiment, colours = sentiment_colours)
plot_group_sentiment <- function(data = data,
                                 group_var = topic,
                                 sentiment_var = sentiment,
                                 colours = sentiment_colours) {

  library(tidyverse)
  sentiment_colours <- c("NEGATIVE" = "#8b0000",
                         "NEUTRAL" = "#808080",
                         "POSITIVE" = "#008b00")
  data %>%
    dplyr::count({{group_var}}, {{sentiment_var}}) %>%
    dplyr::add_count({{group_var}}, wt = n) %>%
    dplyr::mutate(percent = n / nn * 100) %>%
    ggplot2::ggplot(aes(x = {{group_var}}, y = percent, fill = {{sentiment_var}})) +
    ggplot2::geom_col() +
    ggplot2::geom_text(aes(label = paste0(n)),
                       position = ggplot2::position_stack(vjust = 0.5),
                       color = "white") +
    ggplot2::theme_minimal() +
    ggplot2::coord_flip() +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(fill = NULL, y = NULL, x = "Groups") +
    ggplot2::scale_fill_manual(values = sentiment_colours)
}
