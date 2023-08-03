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
count_group_sentiment <- function(data = data,
                                  group_var = group_var,
                                  sentiment_var = sentiment_var) {

  group_sym <- rlang::ensym(group_var)
  sent_sym <- rlang::ensym(sentiment_var)

  grouped_data <- data %>%
    dplyr::group_by(!!group_sym, stringr::str_to_title(!!sent_sym)) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::ungroup()

  grouped_data <- grouped_data %>%
    dplyr::group_by(!!group_sym) %>%
    dplyr::mutate(percent = count / sum(count) * 100) %>%
    dplyr::mutate(percent = round(percent, digits = 1))

  return(grouped_data)

}
