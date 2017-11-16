is_stop_word <- function(word) {
  sum(stop_words == word) >= 1
}