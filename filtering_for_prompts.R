library(rio)
library(tidyverse)

setwd()

#Creating a datafilter before and after the two interventions. R=Restricted mode and Q= Quarantine
df <- import("Dataset.Rdata")

`%nin%` = Negate(`%in%`)
Q <- as.Date("2019-06-26")
R <- as.Date("2020-02-26")

datefilter <- subset(df, (date >= (Q - 50) & date <= (Q + 50)) 
                     | (date >= (R - 50) & date <= (R + 50)))

readydata <- datefilter %>% 
  filter(comment %nin% c("[deleted]", "[removed]"))

td <- readydata[readydata$dataset=="r/The_Donald",]
td_date <- subset(td_filter, date <=R)

users <- readydata[readydata$dataset=="Users",]

td_restricted <- subset(td_date, (date >= (R - 50)))


##
td_quarantine <- subset(td_filter, (date >= (Q - 50) & date <= (Q + 50)))

# Identify users who have commented before and after 2019-06-26
before <- unique(td_quarantine$username[td_quarantine$date < as.Date("2019-06-26")])
after <- unique(td_quarantine$username[td_quarantine$date > as.Date("2019-06-26")])
users <- intersect(before, after)

# Filter the original dataframe to include only those users
td_quarantine_filter <- td_quarantine[td_quarantine$username %in% users, ]

tdanalysis_dataset <- td_quarantine_filter

write.csv(tdanalysis_dataset, "TD_analysis_dataset.csv")


#User dataset before and after
beforeQ <- unique(users$username[users$date > Q-50 & users$date < Q])
afterQ <- unique(users$username[users$date < Q+50 & users$date > Q])
userfilter <- intersect(beforeQ, afterQ)

users_filter_q  <- users[users$username %in% userfilter, ]

beforeR <- unique(users$username[users$date > R-50 & users$date < R])
afterR <- unique(users$username[users$date < R+50 & users$date > R])
userfilter <- intersect(beforeR, afterR)

users_filter_r <- users[users$username %in% userfilter, ]

usersdataset_analysis <- rbind(users_filter_r, users_filter_q)

write.csv(usersdataset_analysis, "users_analysis_dataset.csv")
