library(dplyr)
library(ggplot2)
library(stringr)
library(tm)
library(SnowballC)
library(tidyr)
library(tidytext)
library(wordcloud)
library(plotly)

#  for ggplot
themeObject <- function() {
  return (theme(
    plot.subtitle = element_text(color="#666666"),
    plot.title = element_text(family="Verdana"),
    axis.text.x = element_text(family = "Verdana", size = 10),
    axis.text.y = element_text(family = "Verdana", size = 12)
  ))
}

geniusLyricsUnmodified <- read.csv('lyrics_with_dates.csv', stringsAsFactors = F)

str(geniusLyricsUnmodified)

# make appropriate changes to columns and clean lyrics scrapped from web. And remove Despacito. (NAFIG)
geniusLyrics <- geniusLyricsUnmodified %>%
  mutate(Views = as.numeric(str_remove_all(Views, pattern = 'M'))) %>%
  mutate(Release.Date = as.Date(Release.Date, format = '%B %d, %Y')) %>%
  mutate(Lyrics = str_remove_all(string = Lyrics, pattern = '\\s*\\([^\\)]+\\)')) %>%
  mutate(Lyrics = str_remove_all(string = Lyrics, pattern = '\\s*\\[[^\\]]+\\]')) %>%
  mutate(Lyrics = iconv(Lyrics, to = 'ASCII', sub = '')) %>%
  mutate(Lyrics = trimws(Lyrics)) %>%
  filter(Title != 'Despacito (Remix)') %>%
  mutate(Year = as.factor(format(Release.Date, '%Y'))) %>%
  mutate(Position = row_number())

# get TermDocumentMatrix to analyze most commonly used words.


lyricsByYears <- geniusLyrics %>%
  group_by(Year) %>%
  filter(Position == max(Position))


vs <- VectorSource(lyricsByYears$Lyrics)
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
  summarise(num_of_words = n()) %>%
  arrange(num_of_words, Year) %>%
  top_n(3, num_of_words) %>%
  filter(num_of_words > 20)

commonWordsByYear %>%
  ggplot(aes(x = word, y = num_of_words, fill = word)) +
  facet_grid(. ~ Year, scales = 'free_x') +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x = "Words", y = "Frequency", title = "Most Common Words Per Year") + 
  themeObject() +
  theme(legend.position = "none")

# sentiments

# pos - neg
bingSentiment <- geniusLyrics %>%
  inner_join(get_sentiments("bing"), by = "word")

# weight
afinnSentiment <- geniusLyrics %>%
  inner_join(get_sentiments("afinn"), by = "word")

# other emotions

loughranSentiment <- geniusLyrics %>%
  inner_join(get_sentiments("loughran"), by = "word")

# sentiment over the course of a song
sentimentProgression <- function(title) {
  afinnSentiment %>%
    group_by(Title) %>%
    mutate(position = row_number()) %>%
    filter(Title == title) %>%
    mutate(best = word[which(value == max(value))[1]], worst = word[which(value == min(value))[1]]) %>%
    ggplot(aes(x = position, y = value, fill = value)) + 
    geom_bar(stat = "identity") + 
    geom_label(aes(label = ifelse((word == best | word == worst), word, NA)), na.rm = T) +
    scale_fill_gradient(low="blue", high="orange", guide = guide_colourbar(title = "Sentiment", barwidth = 0.5)) + 
    ylim(-5.5, 5.5) + 
    labs(x = "Progress", y = "Sentiment", subtitle = "Change Of Sentiment Throughout Song", title = title) +
    theme_bw() +
    themeObject()
}

# Mood Wheel for each song
moodWheel <- function(title) {
  afinnSentiment %>%
    filter(Title == title) %>%
    mutate(position = row_number()) %>%
    ggplot(aes(x = position, y = value, fill = value)) +
    geom_bar(stat = 'identity') +
    coord_polar(theta = "x") +
    scale_fill_gradient(low="blue", high="orange", guide = guide_colourbar(title = "Sentiment", barwidth = 0.5)) + 
    labs(y = "") +
    theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
}

moodWheel("Rap God")

negativeAuthors <- afinnSentiment %>%
  group_by(Author) %>%
  summarise(overall_sentiment = sum(value)) %>%
  arrange(desc(overall_sentiment)) %>%
  slice(tail(row_number(), 5))

positiveAuthors <- afinnSentiment %>%
  group_by(Author) %>%
  summarise(overall_sentiment = sum(value)) %>%
  arrange(desc(overall_sentiment)) %>%
  top_n(5, overall_sentiment)

ggplot(negativeAuthors, aes(x = Author, y = overall_sentiment)) + geom_bar(stat = "identity")

# radar plots
em <- loughranSentiment %>%
  filter(Author == "Eminem") %>%
  group_by(Author, sentiment) %>%
  summarise(count = n())

plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) %>%
  add_trace(
    r = em$count,
    theta = em$sentiment,
    name = 'Group A'
  ) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,50)
      )
    )
  )
  
  
