#' count_group_sentiment
#'
#' @param data A tibble or data frame object
#' @param group_var The group variable. For example, topic or audience
#' @param sentiment_var The sentiment variable showing the observations ascribed sentiment based on classification
#'
#' @return A data frame with sentiment distribution counts across each of the group variables
#' @export
#'
#' @examples
#' count_group_sentiment(data = data, group_var = audience, sentiment_var = sentiment)
count_group_sentiment <- function(data,
                                  group_var,
                                  sentiment_var) {

  grouped_data <- data %>%
    group_by({{ group_var }}, {{ sentiment_var }}) %>%
    summarise(count = n()) %>%
    ungroup()

  pivoted_data <- pivot_wider(grouped_data, names_from = {{ sentiment_var }},
                              values_from = count, values_fill = 0)

  pivoted_data <- pivoted_data %>%
    mutate(negative_percent = negative / (positive + neutral + negative) * 100,
           neutral_percent = neutral / (positive + neutral + negative) * 100,
           positive_percent = positive / (positive + neutral + negative) * 100)


  return(pivoted_data)
}

