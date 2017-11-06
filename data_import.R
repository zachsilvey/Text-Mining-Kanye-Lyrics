library(readr)
library(tidyr)
library(dplyr)

# Import lyrics data from CSV
lyrics <- read_csv("Data/Kanye Lyrics (alpha2).csv", 
                   col_types = cols(`Run-time` = col_character())) %>%
  # Convert run-time into integer vector representing seconds
  separate("Run-time", c("minutes", "seconds"), sep = ":", convert = TRUE) %>%
  mutate("run_time" = (minutes * 60) + seconds)

# Remove content between [] from lyrics
clean_lyrics <- gsub("\\[[^]]*]", "", lyrics$Lyrics)
lyrics$Lyrics <- clean_lyrics


