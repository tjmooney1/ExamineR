#' clean_text_var
#' @description A function that performs a series of cleaning steps on a text variable. Useful for data processing when dealing with qualitative data
#' @param data A tibble or data frame object
#' @param text_var The text or message variable to analyse
#' @param tolower Whether to lower case the text variable or not?
#' @param remove_hashtags Should the user remove hashtags?
#' @param remove_mentions Should the user remove any user/profile mentions?
#' @param remove_emojis Should the user remove emojis?
#' @param remove_punctuation Should the user remove punctuation?
#' @param remove_digits Should the user remove digits?
#' @param remove_url Remove most forms of URL from the text variable?
#' @param clean_spaces Clean any redundant or pointless spaces in the text variable?
#' @param in_parallel Whether to run the function in parallel (TRUE = faster)
#'
#' @return The data object provided, with a cleaned text variable
#'
#' @export
#'
#' @examples
#' data <- data %>% clean_text_var(text_var = clean_text, tolower = TRUE, remove_hashtags = TRUE, remove_mentions = TRUE, remove_emojis = TRUE, remove_punctuation = TRUE, remove_digits = TRUE, remove_url = TRUE, clean_spaces = TRUE, in_parallel = TRUE)
clean_text_var <- function(data = data,
                          text_var = message,
                          tolower = TRUE,
                          remove_hashtags = TRUE,
                          remove_mentions = TRUE,
                          remove_emojis = TRUE,
                          remove_punctuation = TRUE,
                          remove_digits = TRUE,
                          remove_url = TRUE,
                          clean_spaces = TRUE,
                          in_parallel = TRUE){

  library(tidyverse)
  text_sym <- rlang::ensym(text_var)
  text_quo <- rlang::enquo(text_var)

  domains <- c(".com", ".ly", ".org", ".net", ".us", ".uk", ".co", ".ch")
  http_regex <- "htt(p|ps)\\S+"
  web_regex <- paste0("[:graph:]*(?=(\\", domains, "/))", "|(?<=(\\", domains, "/))[:graph:]*(?![:alnum:])")
  domain_regex <- paste0("(\\", domains, "/)")

  if (remove_hashtags == TRUE) {
    hashtags_regex <- c("(?<=#)[:graph:]*(?![:graph:])|(?<=#)[:graph:]*$", "#")
    message("Removing hashtags from text variable")
  }
  else {
    hashtags_regex <- NULL
  }
  if (remove_mentions == TRUE) {
    mentions_regex <- c("(?<=@)[:graph:]*(?![:graph:])|(?<=@)[:graph:]*$", "@")
    message("Removing user mentions from text variable")
  }
  else {
    mentions_regex <- NULL
  }
  if (remove_emojis == TRUE) {
    emojis_regex <- "[^\001-\177]"
    message("Removing emojis from text variable")
  }
  else {
    emojis_regex <- NULL
  }
  if (remove_punctuation == TRUE) {
    punctuation_regex <- "[:punct:]"
    message("Removing punctuation from text variable")
  }
  else {
    punctuation_regex <- NULL
  }
  if (remove_digits == TRUE) {
    digits_regex <- "[:digit:]"
    message("Removing digits from text variable")
  }
  else {
    digits_regex <- NULL
  }
  names_regex <- c(web_regex, domain_regex, hashtags_regex, mentions_regex, digits_regex, emojis_regex, punctuation_regex, http_regex)
  all_regex <- character(length(names_regex))
  names(all_regex) <- names_regex
  if (tolower) {
    message("Making all text lowercase")
    data <- data %>%
      dplyr::mutate(`:=`(!!text_quo, tolower(!!text_sym)))
  }
  if (in_parallel) {
    options(future.rng.onMisuse = "ignore")
    message("Beginning parallel sessions")
    future::plan(future::multisession(workers = future::availableCores() - 1))

    data <- data %>%
      dplyr::mutate(cut_id = dplyr::row_number(),cuts = cut(cut_id, future::availableCores() - 1))
    data <- data %>%
      dplyr::group_split(cuts) %>%
      furrr::future_map_dfr(~.x %>%
      dplyr::mutate(`:=`(!!text_quo, stringr::str_remove_all(!!text_sym, all_regex))))

    data <- data %>% dplyr::select(-c(cut_id, cuts))
    message("Ending parallel sessions")
    future::plan(future::sequential())
  }
  else {
    data <- data %>%
      dplyr::mutate(`:=`(!!text_quo, stringr::str_remove_all(!!text_sym, all_regex)), `:=`(!!text_quo, stringr::str_squish(!!text_sym)))
  }
  if (remove_url == TRUE) {
    message("Removing urls from text variable")
    data <- data %>%
      dplyr::mutate(`:=`({{text_var}}, stringr::str_remove_all({{text_var}}, "htt(p|ps)\\S+"))) %>%
      dplyr::mutate(`:=`({{text_var}}, stringr::str_remove_all({{text_var}}, "[w]{3}\\.\\S+"))) %>%
      dplyr::mutate(`:=`({{text_var}}, stringr::str_remove_all({{text_var}}, "\\S+\\.[a-z]+\\S+")))
  }
        if (clean_spaces == TRUE) {
          message("Cleaning whitespaces in text variable")
  data <- data %>%
     dplyr::mutate(`:=`({{text_var}},
  stringr::str_trim({{text_var }})), `:=`({{text_var}},
  stringr::str_squish({{text_var}})), `:=`({{text_var}},
  stringr::str_replace_all({{text_var}}, "[:space:]+\\.", ".")), `:=`({{text_var}},
  stringr::str_replace_all({{text_var}}, "[:space:]+\\,", ",")), `:=`({{text_var}},
  stringr::str_replace_all({{text_var}}, "[:space:]+:", ":")), `:=`({{text_var}},
  stringr::str_replace_all({{text_var}}, "[:space:]+;", ",")), `:=`({{text_var}},
  stringr::str_replace_all({{text_var }}, "[:space:]+\\!", "!")), `:=`({{text_var}},
  stringr::str_replace_all({{text_var}}, "[:space:]+\\?", "?")), `:=`({{text_var}},
  stringr::str_replace_all({{text_var}}, "^\\s*$", NA_character_)))
  }
  data <- data %>% dplyr::filter(!is.na(!!text_sym))
  return(data)
}
