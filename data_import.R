library(readr)
library(tidyr)
library(dplyr)
library(stringr)

# Import lyrics data from CSV
lyrics <- read_csv("Data/Kanye Lyrics (beta1).csv", 
                   col_types = cols(`Run-time` = col_character())) %>%
  # Convert run-time into integer vector representing seconds
  separate("Run-time", c("minutes", "seconds"), sep = ":", convert = TRUE) %>%
  mutate("run_time" = (minutes * 60) + seconds) %>%
  mutate(Lyrics = str_replace_all(Lyrics, "\\[[^]]*]", "")) %>%
  select(-minutes, -seconds)

# Remove content between [] from lyrics
# Deprecated, using stringr to do this tidy
#clean_lyrics <- gsub("\\[[^]]*]", "", lyrics$Lyrics)
#lyrics$Lyrics <- clean_lyrics

#lyrics$Album <- as.factor(lyrics$Album)

