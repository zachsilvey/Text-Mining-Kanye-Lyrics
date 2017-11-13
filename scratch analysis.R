library(tidytext)
library(tidyverse)

# Tokenize lyrics by word
tidy_lyrics <- lyrics %>%
  unnest_tokens(word, Lyrics)

# Count occurances of each word token
tidy_lyrics %>%
  count(word, sort = TRUE)

# Remove stopwords
# Count occurances of each word token by album
tidy_lyrics %>%
  anti_join(stop_words) %>%
  count(Album, word, sort = TRUE)

# Plot top 5 words in each album
# The ordering of bars does not work as intended because some words
# are shared among albums
tidy_lyrics %>%
  anti_join(stop_words) %>%
  filter(Album != "NA") %>%
  group_by(Album) %>%
  count(Album, word, sort = TRUE) %>%
  top_n(5) %>%
  ggplot(aes(x = reorder(word, n), n, fill = Album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Album, ncol = 4, scales = "free") +
  coord_flip()

# This works except some stopwords are showing up
# I believe this is because of an encoding issue
tidy_lyrics %>%
  anti_join(stop_words) %>%
  count(Album, word, sort = TRUE) %>%
  bind_tf_idf(word, Album, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(Album) %>%
  top_n(5) %>%
  ggplot(aes(word, tf_idf, fill = Album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Album, ncol = 4, scales = "free") +
  coord_flip()

# Score net sentiment for each album using bag of words
# and Bing lexicon
tidy_lyrics %>%
  inner_join(get_sentiments("bing")) %>%
  count(Album, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
