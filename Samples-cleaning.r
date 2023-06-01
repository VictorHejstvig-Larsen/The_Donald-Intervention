setwd("/Users/Tobias_Hyldahl/Desktop/Data science/Eksamen")
rm(list=ls())

#### Packages ####
library(tidyverse)
library(textdata)
library(rio)
library(pbapply)
library(tidytext)

#### Cleaning #####
td <- import ("~/Desktop/Data science/Eksamen/TDGPT_clean.csv")
td2 <- import ("~/Desktop/Data science/Eksamen/TDGPT_restricted_clean.csv")
users <- import ("~/Desktop/Data science/Eksamen/usersGPT_clean.csv")
patriots <- import ("~/Desktop/Data science/Eksamen/patriotsGPT_clean.csv")

names(td2)[names(td2) == "V2"] <- "username"
names(td2)[names(td2) == "V3"] <- "id"
names(td2)[names(td2) == "V4"] <- "comment"
names(td2)[names(td2) == "V5"] <- "date"
names(td2)[names(td2) == "V6"] <- "score"
names(td2)[names(td2) == "V7"] <- "gold"
names(td2)[names(td2) == "V8"] <- "subreddit"
names(td2)[names(td2) == "V9"] <- "dataset"
names(td2)[names(td2) == "V10"] <- "quarantine"
names(td2)[names(td2) == "V11"] <- "restricted_mode"
names(td2)[names(td2) == "V12"] <- "ban"
names(td2)[names(td2) == "V13"] <- "GPThate"

td2$GPThate_clean <- gsub(".*: (Yes|No)", "\\1", td2$GPThate)
td2$hatefull <- ifelse(td2$GPThate_clean == "Yes", 1, 0)
td2 <- subset(td2, select = -GPThate_clean)

td$V1 <- NULL
td$V1 <- NULL
td2$V1 <- NULL
td2$V1 <- NULL
td2$V1.1 <- NULL
users$V1 <- NULL
users$V1 <- NULL
patriots$V1 <- NULL

td <- td %>% mutate(sample = "r/The_Donald")
td2 <- td2 %>% mutate(sample = "r/The_Donald")
users <- users %>% mutate(sample = "Users")
patriots <- patriots %>% mutate(sample = "Patriots")

Samples <- bind_rows(td, td2, users, patriots)
colnames(Samples)[colnames(Samples) == "hatefull"] <- "hateful"
rm(td, users, patriots, td2)

#### Save ####
save(Samples,file="Samples.RData")

