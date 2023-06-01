setwd("/Users/Tobias_Hyldahl/Desktop/Data science/Eksamen")
rm(list=ls())

#### Packages ####
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)

#### Load data ####
df <- import ("~/Desktop/Data science/Eksamen/Dataset.rdata")

#### New dataframe ####
subset <- df

#### r/The_Donald comments from the User dataset ####
subset$dataset[subset$dataset == "Users" & subset$subreddit == "The_Donald"] <- "r/The_Donald"

#### Time period ####
subset <- subset %>%
  mutate(date = as.Date(date)) %>%
  filter(!(date > as.Date("2020-08-30") & dataset == "Users"))
subset <- subset %>%
  mutate(date = as.Date(date)) %>%
  filter(!(date > as.Date("2020-08-30") & dataset == "patriots.win"))
subset <- subset %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2019-02-10"))

#### Aggregate data ####
data_grouped <- subset %>%
  mutate(date = as.Date(date)) %>%
  group_by(date, dataset) %>%
  summarize(num_comments = n(), .groups = 'drop')

#### Handle outliers ####
data_grouped <- data_grouped %>%
  mutate(num_comments = if_else(num_comments > 20000, as.numeric(10000), as.numeric(num_comments)))

data_grouped <- data_grouped %>%
  mutate(num_comments = if_else(dataset == "r/The_Donald" & date < "2020-02-25" & num_comments < 4000, as.numeric(10000), 
                                if_else(num_comments > 20000, as.numeric(10000), as.numeric(num_comments))))

#### Figure 1 ####
data_grouped$ordered_dataset <- factor(data_grouped$dataset, levels = c("r/The_Donald", "Users", "patriots.win"))

ggplot(data_grouped, aes(x = date, y = num_comments, color = ordered_dataset, group = dataset)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2019-06-29"), linetype = "dashed", color = "black", size = .55) +
  geom_vline(xintercept = as.Date("2019-11-20"), linetype = "dashed", color = "black", size = .55) +
  geom_vline(xintercept = as.Date("2020-02-26"), linetype = "dashed", color = "black", size = .55) +
  geom_vline(xintercept = as.Date("2020-06-29"), linetype = "dashed", color = "black", size = .55) +
  labs(title = "",
       x = "",
       y = "Number of comments per day") +
  scale_color_manual(values = c("#f37735", "#00BFC4", "#00b159"),
                     labels = c("r/The_Donald", "Same users on\nother subreddits", "Same users\non patriots.win"),
                     name = "") +
  scale_x_date(breaks = as.Date(c("2019-06-29", "2019-11-20", "2020-02-26", "2020-06-29")),
               labels = c("Quarantine\n29 June, 2019","patriots.win opens\n20 November, 2019","Restricted mode\n26 February, 2020", "Ban\n29 June, 2020")) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 40000, by = 2500)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(1.25, 1.17),
        legend.justification = c(2, 1),
        legend.background = element_blank(),
        legend.title = element_text(margin = margin(b = 12, t = 12, unit = "pt")),
        legend.spacing = unit(0.2, "cm"),
        legend.box.just = "right",
        legend.text = element_text(size = 8),
        plot.margin = margin(0, 50, 0, 5.5, "pt"),
        axis.text = element_text(size = 7.5))

