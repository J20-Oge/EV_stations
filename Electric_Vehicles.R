# OSANWUTA OGECHUKWU JASPER 
# MsBA1 
# TEXT ANALYTICS: R 
# BUSINESS REPORT 


# Loading required packages 
library(rtweet)
library(dplyr)
library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)
library(reshape2)
library(stringr)

# STEP 1: Searching for tweets 

#### FIRST SEARCH############
ev_ca <- search_tweets(  #ev_ca == electric cars 
  " #electriccar",
  n= 100, include_rts = FALSE
)


### SECOND SEARCH ###
ev_ch <- search_tweets( #ev_ch == electric car chargers 
  " #EVcharging",
  n= 100, include_rts = FALSE
)


# STEP 2: Processing tweet to tidy text, Cleaning text and Tokenizing 

# Processing each tweet to tidy text 
ev_ca_tweets <- ev_ca %>% 
  # selecting two variables, screen name and text
  select(screen_name, text)

ev_ca_tweets


# Processing each tweet to tidy text 
ev_ch_tweets <- ev_ch %>%
  # selecting two variables, screen name and text
  select(screen_name, text)

ev_ch_tweets


# Cleaning the text 
head(ev_ca_tweets$text)
ev_ca_tweets$clean_text1 <- gsub("http\\s+", " ", ev_ca_tweets$text)
ev_ca_tweets$clean_text1 <- gsub("https?://.+", " ", ev_ca_tweets$text)
ev_ca_tweets$clean_text1 <- gsub("t.co", " ", ev_ca_tweets$text)


# Tokenizing 
ev_ca_tweets_1 <- ev_ca_tweets %>% 
  select(clean_text1) %>% 
  unnest_tokens(word, clean_text1)

# Removing stop words 
ev_ca_cleaned_tweets <- ev_ca_tweets_1 %>% 
  anti_join(stop_words)



# Cleaning the text 
head(ev_ch_tweets$text)
ev_ch_tweets$clean_text2 <- gsub("http\\s+", " ", ev_ch_tweets$text)
ev_ch_tweets$clean_text2 <- gsub("https?://.+", " ", ev_ch_tweets$text)
ev_ch_tweets$clean_text2 <- gsub("t.co", " ", ev_ch_tweets$text)



# Tokenizing 
ev_ch_tweets_1 <- ev_ch_tweets %>% 
  select(clean_text2) %>% 
  unnest_tokens(word, clean_text2)

# Removing stop words 
ev_ch_cleaned_tweets <- ev_ch_tweets_1 %>% 
  anti_join(stop_words)




# Doing a word frequency to check if there is any other unimportant 
#word(s) to remove 

ev_ca_cleaned_tweets %>% 
  count(word, sort = TRUE)

ev_ch_cleaned_tweets %>% 
  count(word, sort = TRUE)



# STEP 3: Sentiment Analysis (nrc lexicon) 

## For ev_ca 
ev_ca_nrc <-ev_ca_cleaned_tweets %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y="contribution to sentiment", x=NULL,
       title = "Electric Cars Sentiment - nrc Lexicon") +
  coord_flip() + theme_bw()
  
print(ev_ca_nrc)

## For ev_ch 
ev_ch_nrc <-ev_ch_cleaned_tweets %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y="contribution to sentiment", x=NULL,
       title = "Electric Car Chargers Sentiment - nrc Lexicon") +
  coord_flip() + theme_bw()

print(ev_ch_nrc)
  


  

  
  

  
  
  

  













