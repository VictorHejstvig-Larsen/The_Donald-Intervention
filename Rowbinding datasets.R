library(rio)
library(ggplot2)
library(tidyverse)
library(xlsx)

`%nin%` = Negate(`%in%`)

setwd("C:/Users/Victor/OneDrive - Aarhus universitet/Kodning/Python/PycharmProjects/DS_scraping_pushshift")

#Function to bind different iterations of the scrape, since we did it over a bunch of times. The function takes a list
#consisting of each iteration and merges each iteartion and removes duplicate rows, since we had a lot of duplicate comments.

bind <- function(...)
{
  combined <- do.call(rbind,list(...))
  
  unique_data <- unique(combined)
  
  return(unique_data)
}

df <- bind(dfpast,dfholes,df)

#write.csv(
# df,
#  "theDonaldComments.csv")

df <- import("theDonaldComments.csv")

df[1] <- NULL

Userlist <- data.frame(unique(df$Author))
colnames(Userlist)[1] <- "Author"

write.csv(df, "theDonaldComments.csv")

rm(df)


#Same idea as the other function but for the comments posted by the list of active users on the_donald.
#The function combines each iteration of the scrape, removes duplicates, removes any authors that were not in the original
#userlist consisting of the active users from the_donald and lastly filters out dates after august 2020.

subsetusers <- function(...)
{
  combined <- do.call(rbind,list(...))
  unique_data <- unique(combined)
  author_check <- unique_data %>% 
    semi_join(Userlist, by = c("Author")) %>% 
    filter(`Publish Date` < "2020-08-31")
  return(author_check)
}

subset <- import("unlceanusercomments.csv")

u <- import("td_userComments.csv")
u[,1] <- NULL

u1 <- subsetusers(u, subset)


#write.csv(u1, "td_userComments.csv")








