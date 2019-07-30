library(dplyr)
library(ggplot2)
library(stringr)
library(tm)
library(SnowballC)
library(tidyr)
library(tidytext)

geniusLyrics <- read.csv('lyrics_with_dates.csv', stringsAsFactors = F)

str(geniusLyrics)

# make appropriate changes to columns and clean lyrics scrapped from web. And remove Despacito. (NAFIG)
geniusLyrics <- geniusLyrics %>%
  mutate(Views = as.numeric(str_remove_all(Views, pattern = 'M'))) %>%
  mutate(Release.Date = as.Date(Release.Date, format = '%B %d, %Y')) %>%
  mutate(Lyrics = str_remove_all(string = Lyrics, pattern = '\\s*\\([^\\)]+\\)')) %>%
  mutate(Lyrics = str_remove_all(string = Lyrics, pattern = '\\s*\\[[^\\]]+\\]')) %>%
  mutate(Lyrics = iconv(Lyrics, to = 'ASCII', sub = '')) %>%
  mutate(Lyrics = trimws(Lyrics)) %>%
  filter(Title != 'Despacito (Remix)') %>%
  mutate(Year = as.factor(format(Release.Date, '%Y')))

# get TermDocumentMatrix to analyze most commonly used words.
vs <- VectorSource(geniusLyrics$Lyrics)
cp <- VCorpus(vs)

tMatrix <- TermDocumentMatrix(cp, control = list(
  removePunctuation = T,
  stopwords = T,
  removeNumbers = T
))

inspect(tMatrix)

stopwordsDf <- data.frame(word = stopwords('en'), stringsAsFactors = F)

# remove stop words from lyrics
geniusLyrics <- geniusLyrics %>%
  unnest_tokens(word, Lyrics) %>%
  anti_join(stopwordsDf, by = "word")

commonWordsByYear <- geniusLyrics %>%
  group_by(Year, word) %>%
  summarise(num_of_words = n())
