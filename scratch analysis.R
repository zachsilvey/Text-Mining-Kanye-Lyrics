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

lyrics <- read_csv("Data/Kanye Lyrics (beta2).csv", 
                   col_types = cols(`Run-time` = col_character())) %>%
  separate("Run-time", c("minutes", "seconds"), sep = ":", convert = TRUE) %>%
  mutate("run_time" = (minutes * 60) + seconds) %>%
  mutate(Lyrics = str_replace_all(Lyrics, "\\[[^]]*]", "")) %>%
  mutate(Album = as.factor(Album)) %>%
  mutate(Album = factor(Album, levels(Album)[c(5,3,2,1,4,7,8,6)]))
  select(-minutes, -seconds)

glimpse(lyrics)

# Plot top 10 terms per album
lyrics %>%
  unnest_tokens(word, Lyrics) %>%
  anti_join(stop_words) %>%
  count(Album, word, sort = TRUE) %>%
  filter(Album != "NA") %>%
  group_by(Album) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ Album, scales = "free", ncol = 3)

# Plot top 10 terms by tf-idf for each album
lyrics %>%
  filter(Skit == "N") %>%
  unnest_tokens(word, Lyrics) %>%
  count(Album, word) %>%
  bind_tf_idf(word, Album, n) %>%
  group_by(Album) %>%
  top_n(10, tf_idf) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = Album)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ Album, scales = "free", ncol = 3) +
  labs(x = "term", y = "tf-idf", title = "Term Frequency-Inverse Document Frequency Among Kanye West Albums")

term_counts_album <- lyrics %>%
  unnest_tokens(word, Lyrics) %>%
  anti_join(stop_words) %>%
  count(Album, word, sort = TRUE) %>%
  group_by(Album) %>%
  top_n(10)

album_totals <- lyrics %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Album) %>%
  count(word) %>%
  summarise(total = sum(n))

term_counts_album %>%
  inner_join(album_totals) %>%
  mutate(freq = n/total) %>%
  arrange(-freq)

album_fac <- as.factor(lyrics$Album)

album_fac <- factor(album_fac, levels(album_fac)[c(5,3,2,1,4,7,8,6)])

levels(album_fac)

album_words <- lyrics %>%
  unnest_tokens(word, Lyrics) %>%
  count(Album, word, sort = TRUE) %>%
  ungroup()

album_totals <- album_words %>%
  group_by(Album) %>%
  summarize(total = sum(n))

album_words <- album_words %>%
  inner_join(album_totals) %>%
  mutate(freq = n / total)

album_words %>%
  anti_join(stop_words) %>%
  filter(Album != "NA") %>%
  group_by(Album) %>%
  summarize(max = max(freq))

# Trying to independently order bars within this plot
album_words %>%
  group_by(Album) %>%
  top_n(10, freq) %>%
  ungroup() %>%
  arrange(word, freq) %>%
  mutate(order = row_number()) %>%
  ggplot(aes(order, freq, fill = Album)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ Album, scales = "free", ncol = 3) +
  scale_x_continuous(
    breaks = order,
    labels = word,
    expand = c(0,0)
  )
