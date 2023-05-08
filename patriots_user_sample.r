#### Sample til ChatGPT ####

rm(list=ls())
setwd("/Users/Tobias_Hyldahl/Desktop/Data science/Eksamen")

# Pakker #
library(rio)
library(tidyverse)

# Load data #
df <- import ("~/Desktop/Data science/Eksamen/Dataset.rdata")

# Ingen deleted/removed #
df <- df[df$comment != "[deleted]", ]
df <- df[df$comment != "[removed]", ]

#### Sample 3: Alle users på patriots.win ####
ss <- df

# Tidsperiode #
ss <- ss[!(ss$restricted_mode < -50 | ss$restricted_mode > 50), ]

# Identificér users, der migrerer efter treatmenttidspunktet #
unique_users <- unique(ss$username[ss$dataset == "patriots.win"])
ss_patriots <- subset(ss, dataset == "patriots.win")
restricted_users <- unique(ss_patriots$username[ss_patriots$restricted_mode >= -88 & ss_patriots$restricted_mode < 0])
unique_users <- setdiff(unique_users, restricted_users) #Fjernusers, der migrerer før restricted mode

ss <- ss[ss$username %in% unique_users, ] # 

# Kun usernames der optræder før og efter intervention #
before_usernames <- unique(ss$username[ss$restricted_mode < 0])
after_usernames <- unique(ss$username[ss$restricted_mode >= 0])
common_usernames <- intersect(before_usernames, after_usernames)
ss <- ss[ss$username %in% common_usernames, ]
rm(before_usernames, after_usernames, common_usernames)

sample3 <- ss # 99.184, faktisk POPULATIONSDATA! :D

write.csv(sample3,file="patriot-users.csv")

