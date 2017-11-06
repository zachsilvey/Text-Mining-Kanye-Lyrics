library(tidytext)
library(dplyr)
library(ggplot2)

tidy_lyrics <- lyrics %>%
  unnest_tokens(word, Lyrics)

tidy_lyrics %>%
  count(word, sort = TRUE)

tidy_lyrics %>%
  anti_join(stop_words) %>%
  count(Album, word, sort = TRUE)

tidy_lyrics %>%
  filter(Album != "NA") %>%
  anti_join(stop_words) %>%
  group_by(Album) %>%
  count(word, sort = TRUE) %>%
  top_n(5, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = Album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Album, ncol = 4, scales = "free") +
  coord_flip()

tidy_lyrics %>%
  count(Album, word, sort = TRUE) %>%
  bind_tf_idf(word, Album, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  top_n(20) %>%
  ggplot(aes(word, tf_idf, fill = Album)) +
  geom_col() +
  facet_wrap(~ Album, ncol = 2, scales = "free") +
  coord_flip()
