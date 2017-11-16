library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(tidyverse)

# Import lyrics data from CSV
lyrics <- read_csv("Data/Kanye Lyrics (beta2).csv", 
                   col_types = cols(`Run-time` = col_character())) %>%
  separate("Run-time", c("minutes", "seconds"), sep = ":", convert = TRUE) %>%
  mutate("run_time" = (minutes * 60) + seconds) %>%
  mutate(Lyrics = str_replace_all(Lyrics, "\\[[^]]*]", "")) %>%
  mutate(Album = as.factor(Album)) %>%
  mutate(Album = factor(Album, levels(Album)[c(5,3,2,1,4,7,8,6)])) %>%
  select(-minutes, -seconds)
# Remove content between [] from lyrics
# Deprecated, using stringr to do this tidy
#clean_lyrics <- gsub("\\[[^]]*]", "", lyrics$Lyrics)
#lyrics$Lyrics <- clean_lyrics

#lyrics$Album <- as.factor(lyrics$Album)

lyrics <- read_csv("Data/Kanye Lyrics (beta2).csv",
                   col_types = cols(Album = col_factor(levels = c("The College Dropout", 
                                                                  "Late Registration", "Graduation", 
                                                                  "808s & Heartbreak", "My Beautiful Dark Twisted Fantasy", 
                                                                  "Watch the Throne", "Yeezus", "The Life of Pablo")))) %>%
  mutate(Lyrics = str_replace_all(Lyrics, "\\[[^]]*]", ""))

lyrics <- read_csv("Data/Kanye Lyrics (beta2).csv", 
                   col_types = cols(Album = col_factor(levels = c("The College Dropout", 
                                                                  "Late Registration", "Graduation", 
                                                                  "808s & Heartbreak", "My Beautiful Dark Twisted Fantasy", 
                                                                  "Watch the Throne", "Yeezus", "The Life of Pablo"))))

lyrics <- read_csv("Data/Kanye Lyrics (beta2).csv", 
                   col_types = cols(Album = col_factor(levels = c("The College Dropout", 
                                                                  "Late Registration", "Graduation", 
                                                                  "808s & Heartbreak", "My Beautiful Dark Twisted Fantasy", 
                                                                  "Watch the Throne", "Yeezus", "The Life of Pablo")))) %>%
  mutate(Lyrics = str_replace_all(Lyrics, "\\[[^]]*]", ""))

glimpse(lyrics)