plot_word_cloud <- function(text_data){

  stop_words <- tm::stopwords(kind = "en")

  word_freq <- base::table(text_vector) %>%
    tm::removeWords(text_vector, stopwords)

  wordcloud::wordcloud(base::names(word_freq), freq = word_freq, random.order = FALSE)
}

text_vector <- Hmisc::Cs(this, is, also, a, vector, it, wasnt, as, bad, to, make, but, still, long, I, guess)


