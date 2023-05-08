rm(list=ls())
library(tidyverse)
library(RSelenium)
library(wdman)
library(rvest)

# Creating a empthy dataframe
df <- data.frame(comment = character(0),
                 author= character(0), 
                 date = character(0), 
                 upvotes = character(0),
                 downvotes = character(0),
                 link = character(0))


### Scraping comments directly from lists of users
# Opening dataset from The_Donald to get list of users
td_userComments <- read.csv("C:/Users/ebben/OneDrive - Aarhus Universitet/Dokumenter/Uni/Data science/Exam/td_userComments.csv")
users <- unique(td_userComments$Author)
rm(td_userComments)

# Taking a sample of 5000 users
set.seed(1889)
user_sample <- sample(users, 5000)

# Creating a sequence to draw from to vary wait time.
x <- c(seq(0.4, 2, by = 0.2))

# Creating strings to combine into URLs
baseurl <- "https://patriots.win/u/"
pageurl <- "/?type=comment&sort=new&page="

# Setting up the headless driver, which is a lot faster than firefox or chrome (docker kept crashing) 
#It requires both having JAVA and phantomjs installed
pjsDrv <- phantomjs(verbose = T, check = FALSE)
pjsDrv$process
rD <- RSelenium::rsDriver()
remDr <- rD[["client"]]

# Checking if it works
remDr$navigate("https://www.patriots.win")
remDr$screenshot(display = T)

# Create a progress bar (We didn't really use it, as we scraped in chunks using a bookmark)
#pb <- txtProgressBar(min = 0, max = length(user_sample), style = 3)

# A quite lengthy loop to scrape the desired content
# Loop over remaining users
for (i in users_rest) {
  # Start on page 1
  j <- 1
  
  while (T) {
    # Create the URL using users and pagenumbers
    url <- paste0(baseurl,i,pageurl,j)
    #Open the URL
    remDr$navigate(url)
    
    ## Setting up a conditional waiting function using the logo on the page
    while (T) {
      
      # Wait 1 second
      Sys.sleep(1)
      #Wait until the logo is loaded
      wait_condition <- tryCatch(remDr$findElement("css", ".logo"),
                                 error = function(e) NULL)
      
      if (!is.null(wait_condition)) {
        break
      }
      
      wait_time <- sample(x, 1)
      # Wait for the drawn number of seconds
      Sys.sleep(wait_time)
    } 
    # Read the source code of the page
    source <- read_html(remDr$getPageSource()[[1]])
    # Scraping of variables of interest using css selectors and breaking the while-loop
    # when there are no comments
    M_downvotes <- source %>%  
      html_elements(".negative span") %>% 
      html_text(trim = T)
    if (length(M_downvotes) == 0) {
      break
    }
    
    M_date <- source %>% 
      html_elements(".timeago") %>% 
      html_attr("datetime")
    # Create a deleted variable so that i get values for all variable when comments are deleted
    M_deleted <- source %>% 
      html_elements(".comment") %>% 
      html_text(trim= T)
    # Giving deleted comments a deleted value
    deleted <- grepl("deleted", M_deleted)
    
    M_comment <- ifelse(deleted, "(deleted)", source %>% 
                          html_elements(".content") %>% 
                          html_text(trim = T))

    M_upvotes <- source %>%  
      html_elements(".positive span") %>% 
      html_text(trim = T)
    
    M_links <- ifelse(deleted, "(deleted)", source %>% 
      html_elements(".actions a:nth-child(1)") %>% 
      html_attr("href"))
    
    # Add the scraped content to df
    df <- df %>%
      add_row(comment = M_comment, author = i, upvotes = M_upvotes, downvotes = M_downvotes, date = M_date, link = M_links)
    # Go to next page
    j <- j+1
  }
  # Print the user
  print(i)
  
  # Increment the progress bar (again we ran it over multiple nights so we did not use progressbars)
  #setTxtProgressBar(pb, which(user_sample == i))
  
}

# Bookmark to be able to take breaks and view progress
# Checking the last user
print(df[nrow(df), ])
which(users == "teddiedilks")
## Saving a new list with remaing users
bookmark <- users_rest[5617:length(users_rest)]

scraped_users <- users_rest[1:5779]

# Making a list of all remaining users
users_rest <- users[!users %in% scraped_users]

# Delete duplicated comments
df <- df %>% 
  distinct(comment,  date, author, link, .keep_all = TRUE)

save(df, file = "C:/Users/ebben/OneDrive - Aarhus Universitet/Dokumenter/Uni/Data science/Exam/patriots_comments.Rdata")
users <- unique(df$author)

## Finding deleted comments
deleted <- df[df$comment == "(deleted)", ]
# Calculating the ratio of deleted comments. 
26034/1594973