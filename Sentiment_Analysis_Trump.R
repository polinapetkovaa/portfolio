# --- Library --- #
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(tidytext)
library(vader)
library(lubridate)
library(tokenizers)
library(textdata)
library(yardstick)
library(ggplot2)

# --- read data --- #

tweets <- read_csv('data/trump_early_2020_tweets.csv')

# add calender week as a column to the dataframe
tweets <-
  tweets %>%
  mutate(cal_week = week(date_est))

# Clean Unnecessary Twitter Junk #

tweets <-
  tweets %>%
  mutate(
    # remove links
    text = str_remove_all(text, "https\\S*"),
    text = str_remove_all(text, "http\\S*"),
    text = str_remove_all(text, "t.co*"),
    # remove mentions
    text = str_remove_all(text, "@\\S*"),
    # remove annoying html stuff
    text = str_remove_all(text, "amp"),
    text = str_remove_all(text, "&S*"),
    text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"),
    text = str_replace_all(text, "<a(.*?)>", " "),
    text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),
    text = str_replace_all(text, "&#[:digit:]+;", " "),
    text = str_remove_all(text, "<[^>]*>"),
    # remove numbers
    text = str_remove_all(text, "[:digit:]"),
    # remove excess whitespace
    text = str_squish(text),
    text = str_trim(text),
    # remove RT for retweets -- keeping retweets in the data
    text = str_remove_all(text, "RT")
  ) %>%
  filter(count_words(text) > 1) %>%
  rownames_to_column("id") %>%
  select(-text_id)

# Tweets to Tidy Tweets #
tidy_tweets<-tweets %>%
  unnest_tokens(word, text, token = "tweets")

# Remove Stopwords #
tidy_tweets<-tidy_tweets %>%
  anti_join(stop_words)

# What does Trump tweet about at different points in the day? #

word_occurence<- tidy_tweets %>%
  count(word, business_hours) %>%
  group_by(word) %>%
  filter(sum(n)>10) %>%
  ungroup() %>%
  pivot_wider(names_from = business_hours,
              values_from = n,
              values_fill=0) %>%
  janitor::clean_names()

#Compute log odds ratio
word_ratios<- word_occurence %>%
  mutate_if(is.numeric, list(~(.+1)/sum(.)+1)) %>%
  mutate(logratio=log(business_hours/non_business_hours))

#Plot the results using 15most unique words per group
word_ratios %>%
  group_by(logratio<0) %>%
  slice_max(abs(logratio), n=15) %>%
  ungroup() %>%
  mutate(word=reorder(word, logratio)) %>%
  ggplot(aes(word,logratio,fill=logratio>0))+
  geom_col()+
  coord_flip()+
  ylab("log odds ratio")+
  scale_fill_discrete(name="", labels=c("business hours", "non-business hours"))

# Vader Sentiment Analysis #
vader_sents<-vader_df(tweets$text)

#Cutting sentiment into positive, negative, and neutral for each tweet that Trump has posted
vader_sents2<-vader_sents %>%
  rowid_to_column("id") %>%
  filter(word_scores != 'ERROR') %>%
  mutate(vader_class=case_when(
    compound < -0.5 ~ "negative",
    compound > 0.5 ~ "positive",
    TRUE ~ "neutral"
  )
  ) %>%
  select(id, vader_class) %>%
  mutate(id=as.character(id))

# Plotting Some Output #
# How does Trump's weekly sentiment evolve over the early stages of 2020?

weekly_sent<- tweets %>%
  inner_join(vader_sents2) %>%
  group_by(cal_week, vader_class, business_hours) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from = vader_class, values_from = n) %>%
  mutate(pos_neg_ratio=positive/negative)

weekly_sent %>%
  ggplot(aes(x=cal_week,
             y=pos_neg_ratio,
             color=business_hours)
  )+
  geom_line()+
  theme_bw()

#takeaway: tweets are angrier in non-business hours

##Source: Deer, Lachlan. 2022. Social Media and Web Analytics: Computing Lecture 3 - Text as Data: Sentiment Analysis. Tilburg University. url = "https://github.com/tisem-digital-marketing/smwa-computing-lecture-text-sentiment"