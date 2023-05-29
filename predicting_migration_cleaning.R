library(tidyverse)
library(httr)
library(jsonlite)
library(text2vec)
library(plotly)
library(rio)
library(Rtsne) 
library(extrafont)
library(ggforce)
library(stringr)
library(ggrepel)
library(tidyverse)
library(tidytext)
library(SentimentAnalysis)
library(textdata)


setwd("C:/Users/Victor/OneDrive/TD_data")

td2 <- read.csv("TDGPT_restricted_clean.csv")
td <- read.csv("TDGPT_clean.csv")

td2$GPThate <- as.numeric(NA)
td$X<- NULL
td2$X <- NULL
td2$V1 <- NULL

colnames(td2) <- colnames(td)

matchyes <- grep("yes", td2$GPThate, ignore.case = TRUE)
matchno <- grep("no", td2$GPThate, ignore.case = TRUE)

td2$hatefull[matchyes] <- 1
td2$hatefull[matchno] <- 0

td <- rbind(td2, td)


td <- na.omit(td)
td$date <- as.Date(td$date)

td <- td[td$date > "2020-01-01",]

tdusers <- td %>% 
  group_by(username) %>% 
  summarize(hate = mean(hatefull))

tdusers$number_of_comments <- td %>% 
  count(username)

tdusers$average_score <- td %>% 
  group_by(username) %>% 
  summarize(score = mean(score))

#sentiment pr user

df_tokens <- td %>%
  unnest_tokens(word, comment)

df_sentiment <- df_tokens %>%
  left_join(get_sentiments("afinn"), by = "word") %>%
  left_join(get_sentiments("bing"), by = "word")

df_sentiment <- df_sentiment %>%
  mutate(
         value = ifelse(sentiment == "negative" & is.na(value), -1, value),  # Replace NA with -1 for negative sentiment
         value = ifelse(sentiment == "positive" & is.na(value), 1, value),# Replace NA with +1 for positive sentiment
         value = ifelse(is.na(value), 0, value))  

usersentiments <- df_sentiment %>% 
  group_by(username) %>% 
  summarize(sentiment=sum(value))

tdusers <- tdusers %>% 
  left_join(usersentiments, by="username")

#Sum mentions in comments
#Muslims/Islam

tdusers$mention_islam <- td %>%
  group_by(username) %>%
  summarize(mention_islam = sum(grepl(paste(c("islam", "muslim", "islamist", "muhammad", "muslims",
                                              "Quran", "Allah", "Hijab", "Ramadan", "Sunni", "Shia",
                                              "Jihad", "Islamic State", "Halal"), collapse = "|"),
                                      comment, ignore.case = TRUE)))

#Trump
tdusers$mention_trump <- td %>%
  group_by(username) %>%
  summarize(mention_trump= sum(grepl(paste(c("Trump", "Donald Trump", "POTUS", "45th", "45th president",
                                             "MAGA", "Trump Tower", "god emperor",
                                             "GEOTUS", "The great Awakening", "Mr. President",
                                             "Commander in chief", "Current president"), collapse = "|"),
                                      comment, ignore.case = TRUE)))
#Conspiracies and deep state
tdusers$mention_conspiracy <- td %>%
  group_by(username) %>%
  summarize(mention_conspiracy= sum(grepl(paste(c("Conspiracy", "Deep state", "Deepstate", "George Soros",
                                                  "Illuminati", "New world order", "The great reset", "
                                                  Pizzagate", "Sandy hook", "Flat earth", "The cabal", "cabal",
                                                  "UFO", "Cover up", "Cover-up", "Secret society","secret societies","
                                                  Mind control", "Rothschild", "Bilderberg", "Big pharma",
                                                  "Anti-vax", "Shadow government", "Crisis actors", "False flags",
                                                  "5G", "New age", "Swamp"), collapse = "|"),
                                     comment, ignore.case = TRUE)))



#The left/democrats
tdusers$mention_left <- td %>%
  group_by(username) %>%
  summarize(mention_left= sum(grepl(paste(c("Democrats","Democratic Party","Left","Left-wing","Liberal",
                                            "Progressives","Socialists","Bernie Sanders","Alexandria Ocasio-Cortez",
                                            "AOC","Nancy Pelosi","Joe Biden","Kamala Harris","Hillary Clinton","Barack Obama",
                                            "Elizabeth Warren","Green New Deal","Medicare for All","Universal Basic Income",
                                            "Libtards","Snowflakes","Social Justice Warriors","Cultural Marxists","PC Police",
                                            "Cancel culture","Virtue signaling","Woke","Radical left","Antifa",
                                            "BLM", "libtards"), collapse = "|"),
                                          comment, ignore.case = TRUE)))
#Fake news / media
tdusers$mention_media_left <- td %>%
  group_by(username) %>%
  summarize(mention_media_left= sum(grepl(paste(c("Fake news","Mainstream media","Lamestream media",
                                             "Corporate media", "Liberal media", "censorship",
                                             "Cancel Culture", "Brainwash", "Brainwashing", "Left-wing media",
                                             "Biased media", "Political correctness", "Propaganda","Indoctrination",
                                             "Indocrinating", "The Washington Times", "NBC", "BBC","NYT", "New York Times"), collapse = "|"),
                                    comment, ignore.case = TRUE)))
#Right wing media
tdusers$mention_media_right <- td %>%
  group_by(username) %>%
  summarize(mention_media_right= sum(grepl(paste(c("Breitbart News", "Infowars", "Fox News",
                                                   "Newsmax", "The Daily Caller", "The Gateway Pundit",
                                                   "The Epoch Times", "Blaze Media", "The Rebel Media",
                                                   "The National Pulse", "American Thinker", "The Federalist",
                                                   "The Daily Wire"), collapse = "|"),
                                     comment, ignore.case = TRUE)))
#Length and words in comments
tdusers$average_length <- td %>%
  group_by(username) %>%
  summarize(average_length = mean(nchar(comment)))

tdusers$average_length_words <- td %>%
  group_by(username) %>%
  summarize(average_length_words = mean(sapply(strsplit(comment, "\\s+"), function(x) mean(nchar(x)))))

tdusers$unique_words <- td %>%
  group_by(username) %>%
  summarize(unique_words = mean(sapply(strsplit(comment, "\\s+"), function(x) mean(nchar(unique(x))))))

tdusers$exclamation <- td %>%
  group_by(username) %>%
  summarize(exclamation = sum(sapply(gregexpr("!", comment), length)))

tdusers$capitalization <- td %>%
  group_by(username) %>%
  summarize(capitalization = sum(sapply(strsplit(comment, "\\s+"), function(x) all(grepl("^[A-Z]+$", x))))) 


patriots <- read.csv("patriotsGPT_clean.csv")

#Creating migration indicator
tdusers$migration <- ifelse(tdusers$username %in% patriots$username, 1,0)

write.csv(tdusers, "migration_prediction_dataset.csv")

