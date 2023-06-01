setwd("/Users/Tobias_Hyldahl/Desktop/Data science/Eksamen")
rm(list=ls())

#### Packages ####
library(tidyverse)
library(rio)

#### Load data ####
data1 <- read_csv("~/Desktop/Data science/Eksamen/theDonaldComments.csv")
data2 <- read_csv("~/Desktop/Data science/Eksamen/td_userComments.csv")
data3 <- import ("~/Desktop/Data science/Eksamen/patriots_comments.rdata")

#### Renaming variables ####
names(data1)[names(data1) == "created_utc"] <- "date"
names(data1)[names(data1) == "body"] <- "comment"
names(data1)[names(data1) == "author"] <- "username"
names(data1)[names(data1) == "gilded"] <- "gold"

names(data2)[names(data2) == "Publish Date"] <- "date"
names(data2)[names(data2) == "Score"] <- "score"
names(data2)[names(data2) == "Body"] <- "comment"
names(data2)[names(data2) == "Author"] <- "username"
names(data2)[names(data2) == "Reddit_Gold"] <- "gold"
names(data2)[names(data2) == "Subreddit"] <- "subreddit"

names(data3)[names(data3) == "author"] <- "username"

#### Deleting variables ####
data1$`...1` <- NULL
data2$`...1` <- NULL
data3$link <- NULL

#### Recoding/generating new variables ####
data2$`id` <- NA
data3$`id` <- NA
data3$`gold` <- NA

data1 <- data1 %>% mutate(subreddit = "The_Donald")
data3$`subreddit` <- NA

data1 <- data1 %>% mutate(dataset = "r/The_Donald")
data2 <- data2 %>% mutate(dataset = "Users")
data3 <- data3 %>% mutate(dataset = "patriots.win")

data1$date[data1$date %in% format(as.POSIXct(data1$date, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:%M:%S")] <- as.POSIXct(data1$date[data1$date %in% format(as.POSIXct(data1$date, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:%M:%S")], format = "%Y-%m-%d %H:%M:%S")
data1$date[data1$date >= 1e10] <- as.POSIXct(as.numeric(data1$date[data1$date >= 1e10]), origin = "1970-01-01")
data1$date <- as.POSIXct(as.numeric(data1$date), origin = "1970-01-01", tz = "UTC")
data1$date <- format(data1$date, "%Y-%m-%d")
data1$date <- as.Date(data1$date, format = "%Y-%m-%d")

data2$date[data2$date %in% format(as.POSIXct(data2$date, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:%M:%S")] <- as.POSIXct(data2$date[data2$date %in% format(as.POSIXct(data2$date, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d %H:%M:%S")], format = "%Y-%m-%d %H:%M:%S")
data2$date[data2$date >= 1e10] <- as.POSIXct(as.numeric(data2$date[data2$date >= 1e10]), origin = "1970-01-01")
data2$date <- as.POSIXct(as.numeric(data2$date), origin = "1970-01-01", tz = "UTC")
data2$date <- format(data2$date, "%Y-%m-%d")
data2$date <- as.Date(data2$date, format = "%Y-%m-%d")

data3$date <- as.Date(data3$date, format = "%Y-%m-%dT%H:%M:%SZ")

data3$upvotes <- as.numeric(data3$upvotes)
data3$downvotes <- as.numeric(data3$downvotes)
data3$score <- data3$upvotes - data3$downvotes
data3 <- data3 %>% 
  select(-downvotes, -upvotes)

#### Merging ####
dataset <- bind_rows(data1, data2, data3)
rm(data1, data2, data3)

#### New variable indicating days from interventions ####
dataset$quarantine <- as.numeric(as.Date(dataset$date) - as.Date("2019-06-29"))
dataset$restricted_mode <- as.numeric(as.Date(dataset$date) - as.Date("2020-02-26"))
dataset$ban <- as.numeric(as.Date(dataset$date) - as.Date("2020-06-29"))

#### Save new dataset ####
save(dataset,file="Dataset.RData")



