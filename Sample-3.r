rm(list=ls())
setwd("/Users/Tobias_Hyldahl/Desktop/Data science/Eksamen")

#### Packages ####
library(rio)
library(tidyverse)

#### Load data ####
df <- import ("~/Desktop/Data science/Eksamen/Dataset.rdata")

#### Removing deleted/removed comments ####
df <- df[df$comment != "[deleted]", ]
df <- df[df$comment != "[removed]", ]

#### New dataframe ####
ss <- df

#### Time period ####
ss <- ss[!(ss$restricted_mode < -50 | ss$restricted_mode > 50), ]

#### Excluding users who migrate before the intervention ####
unique_users <- unique(ss$username[ss$dataset == "patriots.win"])
ss_patriots <- subset(ss, dataset == "patriots.win")
restricted_users <- unique(ss_patriots$username[ss_patriots$restricted_mode >= -88 & ss_patriots$restricted_mode < 0])
unique_users <- setdiff(unique_users, restricted_users)
ss <- ss[ss$username %in% unique_users, ] 

#### Only users with comments both before and after the intervention ####
before_usernames <- unique(ss$username[ss$restricted_mode < 0])
after_usernames <- unique(ss$username[ss$restricted_mode >= 0])
common_usernames <- intersect(before_usernames, after_usernames)
ss <- ss[ss$username %in% common_usernames, ]
rm(before_usernames, after_usernames, common_usernames)

#### Save as csv file ####
write.csv(ss,file="patriot-users.csv")