#' freq_conversations_splits
#' @description  Investigate the most common conversation ids in the data set. A function that makes use of some dplyr functionality, grouping by conversation id, counting and then pulling the message variable for each of the conversation, printing the content in the console for investigation along with the date
#' @param data A tibble or data frame object
#' @param text_var The text or message variable to examine
#' @param conversation_id The variable that contains the conversation id
#' @param url_var The column with each of the posts' respective permalink or URL
#' @param date The date variable to display in the group splits
#' @param n How many of the most frequent conversation ids to inspect
#'
#' @return Prints the message variable in the console of the top n conversations in the data in terms of volume, along with the date of when the message was posted. This might be done to inspect data contents for relevance and cleanliness
#' @export
#'
#' @examples
#' freq_conversations_splits(data = data, text_var = message, conversation_id = conversation_id, url_var = permalink, date = date, n = 10)
#' freq_conversations_top10 <- freq_conversations_splits(data = data, text_var = message, conversation_id = conversation_id, url_var = permalink, date = date, n = 10)
freq_conversations_splits <- function(data = data,
                                      text_var = text_var,
                                      conversation_id = conversation_id,
                                      url_var = url_var,
                                      date = date,
                                      n = n){

  library(tidyverse)
  conversation_counts <- data %>%
    dplyr::count({{conversation_id}}, sort = TRUE)
  conversation_counts_sort <- conversation_counts %>%
    dplyr::slice_max(order_by = n, n = n) %>%
    dplyr::pull({{conversation_id}}) %>%
    sort()

  high_freq_conversations <- data %>%
    dplyr::filter({{conversation_id}} %in% conversation_counts_sort) %>%
    dplyr::select({{text_var}}, {{conversation_id}}, {{url_var}}, {{date}}) %>%
    dplyr::group_split({{conversation_id}})

  base::return(high_freq_conversations)
}
