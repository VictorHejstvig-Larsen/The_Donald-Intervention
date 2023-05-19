library(rio)
library(tidyverse)
library(httr)
library(jsonlite)

setwd("")

TD_sample <- import("TD_sample.csv")
users_sample <- import("users_sample.csv")
patriots_population_analysis <- import("patriots-users.csv")

TD_sample$GPT <- as.character(NA)
users_sample$GPT <- as.character(NA)
patriots_population_analysis$GPT <- as.character(NA)


#Setting up

Sys.setenv(
  OPENAI_API_KEY = 'not a real key'
)
base_url <- "https://api.openai.com/v1/chat/completions"
headers <- c(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")), 
             `Content-Type` = "application/json")
body <- list()
body[["model"]] <- "gpt-3.5-turbo"
body[["max_tokens"]] <- 200

#Angiv hvor mange rækker der skal kodes
batches <- 60

goal <- (nrow(TD_sample) + nrow(users_sample) +nrow(patriots_population_analysis)) / batches

body[["temperature"]] <- 0

token_usage <- 0

loopnr <- 1

total_time <- 0


print("the_donald")
for(row in seq(batches,nrow(TD_sample),batches)){
  
  start_time <- Sys.time()
  
  Sys.sleep(15)
  
  prompt <- "I want you to perform a data annotation task. In your output, 
  I want you to return either yes or
  no, depending on whether you think the following comment is either hateful, toxic,
  offensive.
  I want you to only respond with yes or no.
  Do not provide any other outputs or any explanation for your output."
  
  indices <- 1+(row - batches):(row - 1)
  
  texts <- paste("Comment", 1:length(indices), ": ", TD_sample$comment[indices], " ", collapse = "", sep = "", "Svar: ")
  
  prompt <- paste(prompt, texts, collapse = "","code each comment individually")
  
  if (nchar(prompt) > 13000)
  {
    batches <- 10
    goal <- goal + 1
    indices <- 1+(row - batches):(row - 1)
    texts <- paste("Comment", 1:length(indices), ": ",
    TD_sample$comment[indices], " ", collapse = "", sep = "", "Svar: ")
    prompt <- "I want you to perform a data annotation task. In your output, 
  I want you to return either yes or
  no, depending on whether you think the following comment is either hateful, toxic,
  offensive.
  I want you to only respond with yes or no.
  Do not provide any other outputs or any explanation for your output."
    prompt <- paste(prompt, texts, collapse = "","code each comment individually")
  }

  messages <- list(list(role = "user", content = prompt))
  
  body[["messages"]] <- messages
  
  response <- POST(url = base_url, 
                   add_headers(.headers = headers), 
                   body = body, encode = "json")
  
  completion <- response %>% 
    content(as = "text", encoding = "UTF-8") %>% 
    fromJSON(flatten = TRUE)
  
  if ("error" %in% names(completion)) {
    Sys.sleep(30)
    next
  }
  
  text <- completion$choices$message.content
  
  GPTtd <- unlist(strsplit(text, "Text"))
  
  GPTtd <- strsplit(GPTtd, "\n")[[1]]
  
  TD_sample$GPT[indices] <- GPTtd
  
  subdf <- TD_sample[indices,]
  
  write.table(subdf, file = "TDGPT.csv", sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
  cat("row number", min(rownames(subdf)),":", max(rownames(subdf)), "appended\n")
  
  token_usage <- token_usage + completion$usage$total_tokens
  
  end_time <- Sys.time()
  
  loop_time_seconds <- end_time - start_time
  time <- loop_time_seconds 
  total_time <- total_time + loop_time_seconds 
  average_time <- total_time / loopnr
  
  sekunder <- average_time*(goal-loopnr)
  minuter <- sekunder/60
  timer <- minuter/60
  
  cat("Loop ", loopnr, "/", round(goal),  ". Progress: ", round(loopnr/goal*100), "%.\n")
  
  cat("\033[35mEstimeret tid tilbage: ",
      as.integer(timer),
      "hours, ",
      as.integer(60*(timer - as.integer(timer))),
      "minutes\033[0m\n")
  
  cat(sprintf(
              "%.2f$ brugt\n", token_usage/1000*0.002)) 
  
  loopnr <- loopnr +1 
  
  batches <- 40
  
}

users_sample$comment <- gsub("[^[:alnum:] ?!]", "", users_sample$comment)

#Users
print("users")
for(row in seq(batches ,nrow(users_sample),batches)){
  
  start_time <- Sys.time()
  
  Sys.sleep(15)
  
  prompt <- "I want you to perform a data annotation task. In your output, 
  I want you to return either yes or
  no, depending on whether you think the following comment is either hateful, toxic,
  offensive.
  I want you to only respond with yes or no.
  Do not provide any other outputs or any explanation for your output."
  
  indices <- 1+(row - batches):(row - 1)
  
  texts <- paste("Comment", 1:length(indices), ": ", users_sample$comment[indices], " ", collapse = "", sep = "", "Svar: ")
  
  prompt <- paste(prompt, texts, collapse = "", ".Code each comment individually")
  
  if (nchar(prompt) > 13000)
  {
    batches <- 30
    goal <- goal + 1
    indices <- 1+(row - batches):(row - 1)
    texts <- paste("Comment", 1:length(indices), ": ",
                   TD_sample$comment[indices], " ", collapse = "", sep = "", "Svar: ")
    prompt <- "I want you to perform a data annotation task. In your output, 
  I want you to return either yes or
  no, depending on whether you think the following comment is either hateful, toxic,
  offensive.
  I want you to only respond with yes or no.
  Do not provide any other outputs or any explanation for your output."
    prompt <- paste(prompt, texts, collapse = "","code each comment individually")
  }
  
  if (nchar(prompt) > 13000)
  {
    batches <- 10
    goal <- goal + 1
    indices <- 1+(row - batches):(row - 1)
    texts <- paste("Comment", 1:length(indices), ": ",
                   TD_sample$comment[indices], " ", collapse = "", sep = "", "Svar: ")
    prompt <- "I want you to perform a data annotation task. In your output, 
  I want you to return either yes or
  no, depending on whether you think the following comment is either hateful, toxic,
  offensive.
  I want you to only respond with yes or no.
  Do not provide any other outputs or any explanation for your output."
    prompt <- paste(prompt, texts, collapse = "","code each comment individually")
  }
  
  messages <- list(list(role = "user", content = prompt))
  
  body[["messages"]] <- messages
  
  response <- POST(url = base_url, 
                   add_headers(.headers = headers), 
                   body = body, encode = "json")
  
  completion <- response %>% 
    content(as = "text", encoding = "UTF-8") %>% 
    fromJSON(flatten = TRUE)
  
  if ("error" %in% names(completion)) {
    Sys.sleep(30)
    next
  }
  
  text <- completion$choices$message.content
  
  GPTusers <- unlist(strsplit(text, "Text"))
  
  GPTusers <- strsplit(GPTusers, "\n")[[1]]
  
  users_sample$GPT[indices] <- GPTusers
  
  subdf <- users_sample[indices,]
  
  write.table(subdf, file = "usersGPT.csv", sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
  cat("row number", min(rownames(subdf)),":", max(rownames(subdf)), "appended\n")
  
  token_usage <- token_usage + completion$usage$total_tokens
  
  end_time <- Sys.time()
  
  loop_time_seconds <- end_time - start_time
  time <- loop_time_seconds 
  total_time <- total_time + loop_time_seconds 
  average_time <- total_time / loopnr
  
  sekunder <- average_time*(goal-loopnr)
  minuter <- sekunder/60
  timer <- minuter/60
  
  cat("Loop ", loopnr, "/", round(goal),  ". Progress: ", round(loopnr/goal*100), "%.\n")
  
  cat("\033[35mEstimeret tid tilbage: ",
      as.integer(timer),
      "hours, ",
      as.integer(60*(timer - as.integer(timer))),
      "minutes\033[0m\n")
  
  cat(sprintf("%.2f$ brugt\n", token_usage/1000*0.002))
  
  loopnr <- loopnr +1 
  
  batches <- 60
  

}

patriots_population_analysis$comment <- gsub("[^[:alnum:] ?!]", "", patriots_population_analysis$comment)

#patriots

print("patriots")
for(row in seq(batches, nrow(patriots_population_analysis), batches)){
  
  start_time <- Sys.time()
  
  Sys.sleep(10)
  
  prompt <- "I want you to perform a data annotation task. In your output, 
  I want you to return either yes or
  no, depending on whether you think the following comment is either hateful, toxic,
  offensive.
  I want you to only respond with yes or no.
  Do not provide any other outputs or any explanation for your output."
  
  indices <- 1+(row - 30):(row - 1)
  
  texts <- paste("Comment", 1:length(indices), ": ", patriots_population_analysis$comment[indices], " ", collapse = "", sep = "", "Svar: ")
  
  prompt <- paste(prompt, texts, collapse = "", "code each comment individually")
  
  if (nchar(prompt) > 12000)
  {
    batches <- 30
    goal <- goal + 1
    indices <- 1+(row - batches):(row - 1)
    texts <- paste("Comment", 1:length(indices), ": ",
                   TD_sample$comment[indices], " ", collapse = "", sep = "", "Svar: ")
    prompt <- "I want you to perform a data annotation task. In your output, 
  I want you to return either yes or
  no, depending on whether you think the following comment is either hateful, toxic,
  offensive.
  I want you to only respond with yes or no.
  Do not provide any other outputs or any explanation for your output."
    prompt <- paste(prompt, texts, collapse = "","code each comment individually")
  }
  
  
  if (nchar(prompt) > 12000)
  {
    batches <- 10
    goal <- goal + 1
    indices <- 1+(row - batches):(row - 1)
    texts <- paste("Comment", 1:length(indices), ": ",
                   TD_sample$comment[indices], " ", collapse = "", sep = "", "Svar: ")
    prompt <- "I want you to perform a data annotation task. In your output, 
  I want you to return either yes or
  no, depending on whether you think the following comment is either hateful, toxic,
  offensive.
  I want you to only respond with yes or no.
  Do not provide any other outputs or any explanation for your output."
    prompt <- paste(prompt, texts, collapse = "","code each comment individually")
  }
  
  if ("error" %in% names(completion)) {
    Sys.sleep(30)
    next
  }
  
  messages <- list(list(role = "user", content = prompt))
  
  body[["messages"]] <- messages
  
  response <- POST(url = base_url, 
                   add_headers(.headers = headers), 
                   body = body, encode = "json")
  
  completion <- response %>% 
    content(as = "text", encoding = "UTF-8") %>% 
    fromJSON(flatten = TRUE)
  
  text <- completion$choices$message.content
  
  GPTpatriots <- unlist(strsplit(text, "Text"))
  
  GPTpatriots <- strsplit(GPTpatriots, "\n")[[1]]
  
  patriots_population_analysis$GPT[indices] <- GPTpatriots
  
  subdf <- patriots_population_analysis[indices,]
  
  write.table(subdf, file = "patriotsGPT.csv", sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
  
  cat("row number", min(rownames(subdf)),":", max(rownames(subdf)), "appended\n")
  
  token_usage <- token_usage + completion$usage$total_tokens
  
  end_time <- Sys.time()
  
  loop_time_seconds <- end_time - start_time
  time <- loop_time_seconds 
  total_time <- total_time + loop_time_seconds 
  average_time <- total_time / loopnr
  
  sekunder <- average_time*(goal-loopnr)
  minuter <- sekunder/60
  timer <- minuter/60
  
  cat("Loop ", loopnr, "/", round(goal),  ". Progress: ", round(loopnr/goal*100), "%.\n")
  
  cat("\033[35mEstimeret tid tilbage: ",
      as.integer(timer),
      "hours, ",
      as.integer(60*(timer - as.integer(timer))),
      "minutes\033[0m\n")
  
  cat(sprintf("%.2f$ brugt\n", token_usage/1000*0.002))
  
  loopnr <- loopnr +1 
  
  batches <- 40
  
}

