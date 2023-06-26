test_that("text_var is of class character", {

  data <- read.csv("~/Google Drive/604_sample_10k.csv")
  text_var <- data$text_copy

  expect_true(is.character(text_var), info = "text_var should be character")
})

test_that("stopwords get removed from text_var", {

  stop_words <- tm::stopwords(kind = "smart")

  data <- read.csv("~/Google Drive/604_sample_10k.csv")
  text_var <- data$text_copy
  tokens <- strsplit(tolower(text_var), "\\W+")
  tokens <- data.frame(token = unlist(tokens), stringsAsFactors = FALSE)
  tokens <- tokens %>% dplyr::filter(!token %in% stop_words)
  token <- tokens$token

  expect_no_match(token, "\\bme\\b|\\bmy\\b|\\bmyself\\b", info = "stopwords should now be removed from text variable")
})

test_that("words in text variable are tokenized and defined as a data frame", {

  data <- read.csv("~/Google Drive/604_sample_10k.csv")
  text_var <- data$text_copy
  tokens <- strsplit(tolower(text_var), "\\W+")
  tokens <- data.frame(token = unlist(tokens), stringsAsFactors = FALSE)

  expect_true(is.data.frame(tokens), info = "text should be tokenized as data frame")
})

test_that("words in text variable turn lower case", {

  data <- read.csv("~/Google Drive/604_sample_10k.csv")
  text_var <- data$text_copy
  tokens <- strsplit(tolower(text_var), "\\W+")
  tokens <- data.frame(token = unlist(tokens), stringsAsFactors = FALSE)
  token <- tokens$token

  expect_no_match(token, regexp ="\\b[A-Z]\\w*\\b", info = "tokens should all be lowercase")
})
