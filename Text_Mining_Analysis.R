library(readr)     # read files
library(janitor)   # cleanup var names
library(dplyr)     # data manip
library(tibble)    # work with dataframe
library(tidyr)     # data manip
library(ggplot2)   # plotting
library(stringr)   # work with strings
library(tidytext)  # work with text - main functionality
library(textstem)  # stem words
library(tokenizers) # count words
library(reshape2)  # cast from long to wide and vice versa
library(wordcloud) # plot wordclouds

#Loading the data
df<-read.csv("listings1.csv")


#Keep only the relevant variables
df<-subset(df, select=c(6,7))

#Rename neighboorhood
df<- df %>%
  rename(neighborhood=neighborhood_overview) %>%

#Change observations with missing description to NA
df<- df %>%
  mutate_all(na_if, "")

#Counting characters and approximate word count
df <- df %>%
  mutate(n_char=nchar(description),
         n_words=count_words(description)
         )

#Reduce df to reviews with more than 15 words and less than 250 words
df_words <- df %>%
  filter(n_words > 15, n_words < 250)

#Create histogram
df %>%
  ggplot()+
  geom_histogram(bins=30)+
    aes(x=n_words)+
  scale_x_continuous(breaks = seq(1,650, by = 30))+
  theme_bw()

#Create histogram for reduced df
df_words %>%
  ggplot()+
  geom_histogram(bins=30)+
  aes(x=n_words)+
  scale_x_continuous(breaks = seq(1,250, by = 30))+
  theme_bw()

#Reviews to tokens
tokens<-
  df_words %>%
  rownames_to_column("id") %>%
  unnest_tokens(word, description) %>%
  select(id, word)

#Remove stop words and count common words
common_words_description <- tokens %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  count(sort=TRUE) %>%
  ungroup()

#Custom stop words
custom_stop_words <-
  tibble(
    word=c(
      "î",
      "br",
      "ï",
      "thessaloniki", 
      "bed",
      "îºî",
      "îµî",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "ïzï",
      "îµï",
      "ïfï",
      "ïfî",
      "â",
      "î·ï",
      "îµ",
      "î­ï",
      "îî",
      "î·î",
      "ïfîµ",
      "15",
      "ïoï",
      "24",
      "apartment",
      "flat",
      "house",
      "ïoî",
      "10",
      "ïo",
      "ladadika",
      "tsimiski",
      "meters",
      "byzantine",
      "îºîµî",
      "cafã",
      "îºîµï",
      "roman",
      "apartments",
      "ïfîºîµï",
      "20",
      "thessaloniki's",
      "îºïo",
      "îºî·ï",
      "city",
      "îµïfï",
      "îµïfïfî",
      "city's",
      "thessalonikia",
      "îºî­î",
      "îºï",
      "valaoritou",
      "thessalonikiâ",
      "î'î"),
    lexicon="airbnb"
  )

common_words_description <-
  tokens %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) %>%
  group_by(word) %>%
  count(sort=TRUE) %>%
  ungroup()

#Plot common words
common_words_description %>%
  top_n(100) %>%
  ggplot(aes(n, word)
  ) +
  geom_col() +
  labs(y = NULL)+
  theme_bw()

#Save df with common words as CSV
write.csv(common_words_description,"C:/Users/polina/OneDrive/Documents/thessaloniki/CommonWordsDescription.csv", row.names = FALSE)

#Stemming
tokens_no_stop <- tokens %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words)

tokens_stem<-tokens_no_stop %>%
  mutate(word_lemma=lemmatize_words(word))

common_words2<- tokens_stem %>%
  group_by(word_lemma) %>%
  count(sort=TRUE)%>%
  ungroup()

common_words2 %>%
  top_n(100) %>%
  ggplot(aes(n, word_lemma)
  ) +
  geom_col() +
  labs(y = NULL)+
  theme_bw()


###NEIGHBORHOOD
df2<- df %>%
  mutate(n_char=nchar(neighborhood),
         n_words=count_words(neighborhood)
  )

#Reduce observations
df_words2 <- df2 %>%
  filter(n_words > 15, n_words < 250)

#Create histogram for reduced df
df_words2 %>%
  ggplot()+
  geom_histogram(bins=30)+
  aes(x=n_words)+
  scale_x_continuous(breaks = seq(10,250, by = 30))+
  theme_bw()

#Reviews to tokens
tokens2<-
  df_words2 %>%
  rownames_to_column("id") %>%
  unnest_tokens(word, neighborhood) %>%
  select(id, word)

#Remove stop words and count common words
common_words_description2 <- tokens2 %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  count(sort=TRUE) %>%
  ungroup()

#Using the same custom stop words
common_words_description2 <-
  tokens2 %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) %>%
  group_by(word) %>%
  count(sort=TRUE) %>%
  ungroup()

#Plotting it
common_words_description2 %>%
  top_n(100) %>%
  ggplot(aes(n, word)
  ) +
  geom_col() +
  labs(y = NULL)+
  theme_bw()

#Save df with common words as CSV
write.csv(common_words_description2,"C:/Users/polina/OneDrive/Documents/thessaloniki/NeighborhoodCommonWords.csv", row.names = FALSE)


