#### Are the migrants more toxic to begin with? ####
setwd("/Users/Tobias_Hyldahl/Desktop/Data science/Eksamen")
rm(list=ls())

#### Packages ####
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)
library(rio)

df <- import ("~/Desktop/Data science/Eksamen/Samples.rdata")
df$date <- as.Date(df$date, format = "%Y-%m-%d")
df <- df[complete.cases(df) | !is.na(df$hateful), ]
df <- df[df$comment != "(deleted)", ]
df$dataset[df$dataset == "Users" & df$subreddit == "The_Donald"] <- "r/The_Donald" #Kun fra patriotssamplet

filtered_subset <- subset(df, sample == "Patriots")
unique_usernames <- unique(filtered_subset$username)
df$migrant <- ifelse(df$username %in% unique_usernames, 1, 0)
rm(filtered_subset, unique_usernames)

#### ####
subset <- df

subset <- subset[!(subset$restricted_mode < -51), ]
subset <- subset[!(subset$restricted_mode >= 0), ]


####
data_grouped <- subset %>%
  mutate(date = as.Date(date)) %>%
  group_by(date, migrant, dataset) %>%
  summarize(prop_hateful = mean(hateful)) # calculate the proportion of hateful comments for each day and dataset

ggplot(data_grouped, aes(x = date, y = prop_hateful, color = dataset, group = migrant)) + 
  geom_point(aes(color = ifelse(date >= as.Date("2020-02-26"), "right", "left")), size = 2) + # use points instead of lines and assign color based on date
  geom_vline(xintercept = as.Date("2019-06-29"), linetype = "dashed", color = "black", size = .7) +
  geom_vline(xintercept = as.Date("2020-02-26"), linetype = "dashed", color = "black", size = .7) +
  geom_vline(xintercept = as.Date("2020-06-29"), linetype = "dashed", color = "black", size = .7) +
  labs(title = "",
       x = "",
       y = "Share of hateful comments per day") + 
  scale_color_manual(values = c("#FF0000", "#0000FF", "#00BFC4"), # set colors for each dataset
                     labels = c("", "", ""), # remove legend title for "patriots.win"
                     name = "") +
  scale_x_date(breaks = as.Date(c("2019-06-29", "2020-02-26", "2020-06-29")),
               labels = c("Quarantine\n29 June, 2019", "Restricted mode\n26 February, 2020", "Ban\n29 June, 2020")) +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(1.33, 1),
        legend.justification = c(2, 1),
        legend.background = element_blank(),
        plot.margin = margin(0, 85, 0, 5.5, "pt")) +
  geom_smooth(data = filter(data_grouped, date < as.Date("2020-02-26")), method = "lm", se = FALSE, color = "#FF0000") + # set color for left trend line
  geom_smooth(data = filter(data_grouped, date >= as.Date("2020-02-26")), method = "lm", se = FALSE, color = "#0000FF") + # set color for right trend line
  guides(color = guide_legend(override.aes = list(size=0))) # remove dots in legend

####
subset <- df
subset <- subset[!(subset$restricted_mode < -51), ]
subset <- subset[!(subset$restricted_mode >= 0), ]

new_df <- subset %>%
  group_by(username) %>%
  summarize(
    hateful_share = mean(hateful),
    comment_count = n(),
    mean_migrant = mean(migrant),
    mean_score = mean(score)
  )

#### Comparing averages ####
migrant_1 <- subset(new_df, mean_migrant == 1)
migrant_0 <- subset(new_df, mean_migrant == 0)

# Hateful_share #
mean_1 <- mean(migrant_1$hateful_share)
mean_0 <- mean(migrant_0$hateful_share)
mean_diff <- mean_1 - mean_0
ttest_result <- t.test(new_df$hateful_share ~ new_df$mean_migrant)
p_value <- ttest_result$p.value

# Comment count #
mean_1 <- mean(migrant_1$mean_score)
mean_0 <- mean(migrant_0$mean_score)
mean_diff <- mean_1 - mean_0
ttest_result <- t.test(new_df$mean_score ~ new_df$mean_migrant)
p_value <- ttest_result$p.value

# Print the results
cat("Group 1 mean:", mean_1, "\n")
cat("Group 0 mean:", mean_0, "\n")
cat("Difference in means:", mean_diff, "\n")
cat("p-value:", p_value, "\n")






####
df2 <- import ("~/Desktop/Data science/Eksamen/Dataset.rdata")
df2$deleted <- ifelse(df2$comment %in% c("[removed]", "[deleted]", "(deleted)"), 1, 0)

filtered_subset <- subset(df, sample == "Patriots")
unique_usernames <- unique(filtered_subset$username)
df2$migrant <- ifelse(df2$username %in% unique_usernames, 1, 0)
rm(filtered_subset, unique_usernames)

df2 <- df2[!(df2$restricted_mode < -51), ]
df2 <- df2[!(df2$restricted_mode >= 0), ]
df2 <- df2[!(df2$deleted == 1), ]

aggregated_df <- aggregate(comment ~ username + migrant, data = df2, FUN = length)
names(aggregated_df)[names(aggregated_df) == "comment"] <- "num_comments"

# Subset the data for the groups where migrant is 1 and 0
migrant_1 <- subset(aggregated_df, migrant == 1)
migrant_0 <- subset(aggregated_df, migrant == 0)

# Perform t-test to compare the means
t_test <- t.test(migrant_1$num_comments, migrant_0$num_comments)

# Print the means and the p-value
cat("Mean comments for migrant == 1:", mean(migrant_1$num_comments), "\n")
cat("Mean comments for migrant == 0:", mean(migrant_0$num_comments), "\n")
cat("P-value:", t_test$p.value, "\n")



#### Comparing averages ####
migrant_1 <- subset(df2, mean_migrant == 1)
migrant_0 <- subset(df2, mean_migrant == 0)

# Hateful_share #
mean_1 <- mean(migrant_1$hateful_share)
mean_0 <- mean(migrant_0$hateful_share)
mean_diff <- mean_1 - mean_0
ttest_result <- t.test(new_df$hateful_share ~ new_df$mean_migrant)
p_value <- ttest_result$p.value

# Comment count #
mean_1 <- mean(migrant_1$comment_count)
mean_0 <- mean(migrant_0$comment_count)
mean_diff <- mean_1 - mean_0
ttest_result <- t.test(new_df$comment_count ~ new_df$mean_migrant)
p_value <- ttest_result$p.value


# Print the results
cat("Group 1 mean:", mean_1, "\n")
cat("Group 0 mean:", mean_0, "\n")
cat("Difference in means:", mean_diff, "\n")
cat("p-value:", p_value, "\n")
