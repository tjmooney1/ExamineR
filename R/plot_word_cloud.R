plot_word_cloud <- function(text_data){

  word_freq <- table(text_data)

  wordcloud::wordcloud(names(word_freq), freq = word_freq, random.order = FALSE)
}


text_vector <- Hmisc::Cs(this, is, a, vector, now, it, this, is, a)

plot_word_cloud(text_vector)

