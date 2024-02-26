#' Clean Text Variable Ready for Embedding
#'
#' @param data A tibble or data frame object
#' @param text_var The text variable or documents to be cleaned for the embedding process
#' @param to_lower Whether to make text lowercase, the default option is TRUE
#' @param in_parallel Whether to utilize parallel processing and split the job across cores, in order to process faster. The default option is FALSE
#'
#' @return Returns the data object with the provided text_var cleaned and ready to begin embeddings
#' @export
#'
#' @examples
#' output <- data %>%
#' dplyr::mutate(message_cleaned = message) %>%
#'  text_embeddings_pre_processing(text_var = message_cleaned)
text_embeddings_pre_processing <- function(data,
                                           text_var,
                                           to_lower = TRUE,
                                           in_parallel = FALSE) {
  # sort text variable as symbol
  text_sym <- rlang::ensym(text_var)

  # REGEX patterns for each cleaning step
  hashtags_regex <- c("(?<=#)[:graph:]*(?![:graph:])|(?<=#)[:graph:]*$", "#") # hashtags
  mentions_regex <- c("(?<=@)[:graph:]*(?![:graph:])|(?<=@)[:graph:]*$", "@") # mentions of users
  emojis_regex <- "[^\001-\177]" # emojis
  punctuation_regex <- "[:punct:]" # punctuation
  non_english_regex <- "[^A-Za-z0-9[:punct:][:space:]]" # non-English symbols
  spaces_starting_docs_regex <- "^\\s+" # documents with a space at the start
  digits_starting_docs_regex <- "^[0-9]+\\s*" # documents with digits starting them

  all_regex_names <- c(hashtags_regex, # join all above REGEX patterns
                       mentions_regex,
                       emojis_regex,
                       punctuation_regex,
                       non_english_regex,
                       spaces_starting_docs_regex,
                       digits_starting_docs_regex)
  all_regex <- character(length(all_regex_names))
  names(all_regex) <- all_regex_names

  # REGEX for any bits of text with multiple spaces in a row
  multiple_spaces_regex <- "\\s{2,}"

  # REGEX for URLs and similar
  urls_regex <- "htt(p|ps)\\S+|[w]{3}\\.\\S+|\\S+\\.[a-z]+\\S+"

  # if to_lower is called, which it is by default, make all text lowercase
  if (to_lower == TRUE) {
    data <- data %>%
      dplyr::mutate(!!text_sym := tolower(!!text_sym))
  }

  # if in_parallel is called, split processing across all cores except one
  if (in_parallel == TRUE) {
    options(future.rng.onMisuse = "ignore")
    message("Beginning parallel sessions")
    future::plan(future::multisession(workers = future::availableCores() - 1))

    data <- data %>%
      dplyr::mutate(cuts_id = dplyr::row_number(),cuts = cut(cuts_id, future::availableCores() - 1))
    data <- data %>%
      dplyr::group_split(cuts) %>%
      furrr::future_map_dfr(~.x %>%
                              dplyr::mutate({{text_sym}} := stringr::str_replace_all({{text_sym}}, urls_regex, ""),
                                            {{text_sym}} := stringr::str_remove_all({{text_sym}}, all_regex),
                                            {{text_sym}} := stringr::str_replace_all({{text_sym}}, multiple_spaces_regex," ")))

    # remove those bits where we cut the data before and end parallel session
    data <- data %>% dplyr::select(-c(cuts_id, cuts))
    message("Ending parallel sessions")
    future::plan(future::sequential())
  } else {
    data <- data %>%
      dplyr::mutate({{text_sym}} := stringr::str_replace_all({{text_sym}}, urls_regex, ""),
                    {{text_sym}} := stringr::str_remove_all({{text_sym}}, all_regex),
                    {{text_sym}} := stringr::str_replace_all({{text_sym}}, multiple_spaces_regex," "))

  }

  return(data)
}
