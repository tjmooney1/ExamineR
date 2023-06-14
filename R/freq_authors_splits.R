#' freq_authors_splits
#' @description  Investigate the most active users within a data set, inspecting message variable for contents. A function that makes use of some dplyr functionality, grouping by author, counting and then pulling the message variable for each of the authors, printing the content in the console for investigation
#' @param data A tibble or data frame object
#' @param text_var The text or message variable to examine
#' @param author_var The variable that contains the senders' screen name or author name
#' @param url_var The column with each of the posts' respective permalink or url
#' @param n How many of the most active users in the data set to inspect
#'
#' @return Prints the n number of most frequent authors' along with a selection of their respective message variable in the console to inspect their content, spam posters, bots or irrelevant data should be
#' @export
#'
#' @examples
#' freq_authors_splits(data = data, text_var = message, author_var = sender_screen_name, url_var = permalink, n = 10)
#' freq_authors_top10 <- freq_authors_splits(data = data, text_var = message, author_var = sender_screen_name, url_var = permalink, n = 10)
freq_authors_splits <- function(data = data,
                                text_var = text_var,
                                author_var = author_var,
                                url_var = url_var,
                                n = n){

  library(tidyverse)
  author_counts <- data %>%
    dplyr::count({{author_var}}, sort = TRUE)
  author_counts_sort <- author_counts %>%
    dplyr::slice_max(order_by = n, n = {{n}}) %>%
    dplyr::pull({{author_var}}) %>%
    sort()

  high_freq_authors <- data %>%
    dplyr::filter({{author_var}} %in% author_counts_sort) %>%
    dplyr::select({{text_var}}, {{author_var}}, {{url_var}}) %>%
    dplyr::group_split({{author_var}})

  base::return(high_freq_authors)
}
