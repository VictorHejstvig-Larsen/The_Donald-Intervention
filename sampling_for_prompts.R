library(rio)
library(tidyverse)
library(httr)
library(jsonlite)
`%nin%` = Negate(`%in%`)

setwd("C:/Users/45533/OneDrive - Aarhus universitet/Kodning/Python/PycharmProjects/DS_scraping_pushshift")
TD_analysis <- import("TD_analysis_dataset.csv")

  #Fixing some date stuff
TD_date_subset <- TD_analysis[TD_analysis$date %in% c("2019-06-26", "2019-07-27"),] 
set.seed(111)
TD_date_subset <- TD_date_subset[sample(nrow(TD_date_subset),16000),]
TD_analysis <- TD_analysis[TD_analysis$date %nin% c("2019-06-26", "2019-07-27"),]
TD_analysis <- rbind(TD_analysis,TD_date_subset)
TD_analysis <- TD_analysis[complete.cases(TD_analysis$comment),]

#users
users_analysis <- import("users_analysis_dataset.csv")
users_analysis <- users_analysis[users_analysis$subreddit != "The_Donald",]
users_analysis <- users_analysis[complete.cases(users_analysis$comment),]


patriots_population_analysis <- import("patriots-users.csv")
patriots_population_analysis <- patriots_population_analysis[complete.cases(patriots_population_analysis$comment),]


#Sampling TD
set.seed(9512)
TD_sample <- TD_analysis[sample(nrow(TD_analysis),30000),]

#Ordering batches to not exceed 12000 characters for 40 comments:
# Initialize indices vector 
inds <- 1:nrow(TD_sample)

# Keep taking 40 row subsets until character length exceeds 12000
i <- 1 
while(nchar(paste(TD_sample[i:(i+39),]$comment, collapse = '')) < 11000) {
  i <- i + 40
} 

# Extract indices of rows in last subset 
last_ind <- i:(i+39)  

# Remove last subset indices from full indices vector 
inds <- inds[-c(last_ind)]

# Append last subset indices to end of indices vector
inds <- c(inds, last_ind) 

# Reorder dataframe using indices 
TD_sample <- TD_sample[inds, ]
TD_sample$comment <- gsub("\n", "", TD_sample$comment)

#Users

set.seed(9512)
users_sample <- users_analysis[sample(nrow(users_analysis),70000),]

inds <- 1:nrow(users_sample)
i <- 1 
while(nchar(paste(users_sample[i:(i+39),]$comment, collapse = '')) < 11000) {
  i <- i + 40
} 
last_ind <- i:(i+39)  
inds <- inds[-c(last_ind)]
inds <- c(inds, last_ind) 
users_sample <- users_sample[inds, ]
users_sample$comment <- gsub("\n", "", users_sample$comment)


#Patriots
inds <- 1:nrow(patriots_population_analysis)
i <- 1 
while(nchar(paste(patriots_population_analysis[i:(i+39),]$comment, collapse = '')) < 11000) {
  i <- i + 40
} 
last_ind <- i:(i+39)  
inds <- inds[-c(last_ind)]
inds <- c(inds, last_ind) 
patriots_population_analysis <- patriots_population_analysis[inds, ]
patriots_population_analysis$comment <- gsub("\n", "", patriots_population_analysis$comment)


write.csv(users_sample, "users_sample.csv")
write.csv(TD_sample, "TD_sample.csv")
write.csv(patriots_population_analysis, "patriots-users.csv")
