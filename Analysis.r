setwd("/Users/Tobias_Hyldahl/Desktop/Data science/Eksamen")
rm(list=ls())

#### Packages ####
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)
library(rio)
library(stargazer)

#### Load and clean data ####
df <- import ("~/Desktop/Data science/Eksamen/Samples.rdata")
df$date <- as.Date(df$date, format = "%Y-%m-%d")
df <- df[complete.cases(df) | !is.na(df$hateful), ]
df <- df[df$comment != "(deleted)", ]
df$dataset[df$dataset == "Users" & df$subreddit == "The_Donald"] <- "r/The_Donald"

#### Model 1 ####
subset <- df
subset <- subset[!(subset$subreddit == "The_Donald" & !is.na(subset$subreddit) & subset$restricted_mode >= 0), ]
subset <- subset[!(subset$subreddit == "The_Donald" & !is.na(subset$subreddit) & subset$restricted_mode < -51), ]
subset <- subset[subset$sample != "Users", ]
subset <- subset[subset$dataset != "Users", ]

#### Model 2 ####
subset <- df
subset <- subset[!(subset$subreddit == "The_Donald" & !is.na(subset$subreddit) & subset$restricted_mode >= 0), ]
subset <- subset[subset$sample != "Users", ]
subset <- subset[subset$sample != "r/The_Donald", ]
subset <- subset[subset$dataset != "Users", ]

#### Model 3 and 4 ####
subset <- df
subset <- subset[subset$sample != "r/The_Donald", ]
subset <- subset[subset$sample != "Patriots", ]
subset <- subset[subset$restricted_mode >= -50, ]

#### Model 5 and 6 ####
subset <- df
subset <- subset[subset$sample != "Patriots", ]
subset <- subset[subset$sample != "Users", ]
subset <- subset(subset, !(sample == "r/The_Donald" & quarantine > 51))

#### Treatment dummys ####
subset <- subset %>%
  mutate(treatment = ifelse(restricted_mode < 0, 0, 1))

subset <- subset %>%
  mutate(treatment = ifelse(quarantine < 0, 0, 1))

#### Models without fixed effects ####
resultat <- lm(hateful ~ restricted_mode * treatment, data = subset)
resultat <- lm(hateful ~ quarantine * treatment, data = subset)

stargazer(resultat2, type = "text", star.cutoffs = c(0.05, 0.01, 0.001))

#### Models with fixed effects ####
resultat <- lm(hateful ~ restricted_mode * treatment + as.factor(username), data = subset)
resultat <- lm(hateful ~ quarantine * treatment + as.factor(username), data = subset)

stargazer(resultat, type = "text", omit = "username", star.cutoffs = c(0.05, 0.01, 0.001))

#### Save predicted values ####
subset$predicted <- predict(resultat, newdata = subset)

data_grouped <- subset %>%
  group_by(restricted_mode, dataset) %>%
  summarize(prop_hateful = mean(hateful),
            predicted = mean(predicted))

data_grouped <- subset %>%
  group_by(quarantine, dataset) %>%
  summarize(prop_hateful = mean(hateful),
            predicted = mean(predicted))

#### Figure 5A ####
ggplot(data_grouped, aes(x = restricted_mode, y = prop_hateful, color = dataset, group = dataset)) + 
  geom_point(aes(color = ifelse(restricted_mode >= 0, "right", "left")), size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = .7) +
  labs(title = "",
       x = "",
       y = "") + 
  scale_color_manual(values = c("#f37735", "#00b159"),
                     labels = c("r/The_Donald", "patriots.win"),
                     name = "") +
  scale_x_continuous(breaks = c(-50, 50),
                     labels = c("", "")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = percent_format(), expand = c(0, 0.05)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(1.23, 1.09),
        legend.justification = c(2, 1),
        legend.background = element_blank(),
        plot.margin = margin(0, 85, 0, 5.5, "pt")) +
  geom_line(data = subset(data_grouped, restricted_mode < 0), aes(y = predicted, group = dataset), color = "#f37735", size = 1) +
  geom_line(data = subset(data_grouped, restricted_mode >= 0), aes(y = predicted, group = dataset), color = "#00b159", size = 1) +
  guides(color = guide_legend(override.aes = list(size = 1.3)))

#### Figure 5B ####
data_grouped <- data_grouped %>%
  mutate(predicted = ifelse(restricted_mode < 0, -0.0004 * restricted_mode + 0.326, -0.0003 * restricted_mode + 0.332))

ggplot(data_grouped, aes(x = restricted_mode, y = prop_hateful, color = dataset, group = dataset)) + 
  geom_point(aes(color = ifelse(restricted_mode >= 0, "right", "left")), size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = .7) +
  labs(title = "",
       x = "",
       y = "") + 
  scale_color_manual(values = c("#f37735", "#00b159"),
                     labels = c("", ""),
                     name = "") +
  scale_x_continuous(breaks = c(-50, -40, -30, -20, 0, 20, 30, 40, 50),
                     labels = c("-50", "-40", "-30", "-20", "Restricted mode\n26 February, 2020", "20", "30", "40", "50")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = percent_format(), expand = c(0, 0.05)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(1.25, 1.09),
        legend.justification = c(2, 1),
        legend.background = element_blank(),
        plot.margin = margin(0, 85, 0, 5.5, "pt")) +
  geom_line(data = subset(data_grouped, restricted_mode < 0), aes(y = predicted, group = dataset), color = "#f37735", size = 1) +
  geom_line(data = subset(data_grouped, restricted_mode >= 0), aes(y = predicted, group = dataset), color = "#00b159", size = 1) + 
  guides(color = guide_legend(override.aes = list(size = 1.3)))

#### Figure 5C ####
ggplot(data_grouped, aes(x = restricted_mode, y = prop_hateful, color = dataset, group = dataset)) + 
  geom_point(aes(color = ifelse(restricted_mode >= 0, "right", "left")), size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = .7) +
  labs(title = "",
       x = "",
       y = "") + 
  scale_color_manual(values = c("#00BFC4", "#00BFC4"),
                     labels = c("Other subreddits"),
                     name = "") +
  scale_x_continuous(breaks = c(-50, 50),
                     labels = c("", "")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = percent_format(), expand = c(0, 0.05)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(1.27, 1.09),
        legend.justification = c(2, 1),
        legend.background = element_blank(),
        plot.margin = margin(0, 85, 0, 5.5, "pt")) +
  geom_line(data = subset(data_grouped, restricted_mode < 0), aes(y = predicted, group = dataset), color = "#00BFC4", size = 1) + 
  geom_line(data = subset(data_grouped, restricted_mode >= 0), aes(y = predicted, group = dataset), color = "#00BFC4", size = 1) + 
  guides(color = guide_legend(override.aes = list(size = 1.3))) 

#### Figure 5D ####
data_grouped <- data_grouped %>%
  mutate(predicted = ifelse(restricted_mode < 0, 0.0002 * restricted_mode + 0.216, -0.0002 * restricted_mode + 0.216 - 0.002))

ggplot(data_grouped, aes(x = restricted_mode, y = prop_hateful, color = dataset, group = dataset)) + 
  geom_point(aes(color = ifelse(restricted_mode >= 0, "right", "left")), size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = .7) +
  labs(title = "",
       x = "",
       y = "Share of hateful comments per day") + 
  scale_color_manual(values = c("#00BFC4", "#00BFC4"),
                     labels = c(""), 
                     name = "") +
  scale_x_continuous(breaks = c(-50, -40, -30, -20, 0, 20, 30, 40, 50),
                     labels = c("-50", "-40", "-30", "-20", "Restricted mode\n26 February, 2020", "20", "30", "40", "50")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = percent_format(), expand = c(0, 0.05)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(1.55, 1.09),
        legend.justification = c(2, 1),
        legend.background = element_blank(),
        plot.margin = margin(0, 85, 0, 5.5, "pt")) +
  geom_line(data = subset(data_grouped, restricted_mode < 0), aes(y = predicted, group = dataset), color = "#00BFC4", size = 1) + # red trend line for left side
  geom_line(data = subset(data_grouped, restricted_mode >= 0), aes(y = predicted, group = dataset), color = "#00BFC4", size = 1) + # blue trend line for right side
  guides(color = guide_legend(override.aes = list(size = 1.3)))

#### Figure 6A ####
ggplot(data_grouped, aes(x = quarantine, y = prop_hateful, color = dataset, group = dataset)) + 
  geom_point(aes(color = ifelse(quarantine >= 0, "right", "left")), size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = .7) +
  labs(title = "",
       x = "",
       y = "Share of hateful comments per day") + 
  scale_color_manual(values = c("#f37735", "#f37735", "#f37735"),
                     labels = c("r/The_Donald", ""), 
                     name = "") +
  scale_x_continuous(breaks = c(-50, -40, -30, -20, 0, 20, 30, 40, 50),
                     labels = c("-50", "-40", "-30", "-20", "Quarantine\n29 June, 2019", "20", "30", "40", "50")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = percent_format(), expand = c(0, 0.05)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(1.23, 1.09),
        legend.justification = c(2, 1),
        legend.background = element_blank(),
        plot.margin = margin(0, 85, 0, 5.5, "pt")) +
  geom_line(data = subset(data_grouped, quarantine < 0), aes(y = predicted, group = dataset), color = "#f37735", size = 1) + 
  geom_line(data = subset(data_grouped, quarantine >= 0), aes(y = predicted, group = dataset), color = "#f37735", size = 1) + 
  guides(color = guide_legend(override.aes = list(size = 1.3))) 

#### Figure 6B ####
data_grouped <- data_grouped %>%
  mutate(predicted = ifelse(quarantine < 0, -0.0002 * quarantine + 0.366, -0.0003 * quarantine + 0.366 + 0.007))

ggplot(data_grouped, aes(x = quarantine, y = prop_hateful, color = dataset, group = dataset)) + 
  geom_point(aes(color = ifelse(quarantine >= 0, "right", "left")), size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = .7) +
  labs(title = "", x = "", y = "") + 
  scale_color_manual(values = c("#f37735", "#f37735", "#f37735"),
                     labels = c("", ""), 
                     name = "") +
  scale_x_continuous(breaks = c(-50, -40, -30, -20, 0, 20, 30, 40, 50),
                     labels = c("-50", "-40", "-30", "-20", "Quarantine\n29 June, 2019", "20", "30", "40", "50")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = NULL, expand = c(0, 0.05),
                     sec.axis = sec_axis(~ ., labels = scales::percent_format())) +  # Add secondary axis
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(1.70, 1.09),
        legend.justification = c(2, 1),
        legend.background = element_blank(),
        plot.margin = margin(0, 85, 0, 5.5, "pt")) +
  geom_line(data = subset(data_grouped, quarantine < 0), aes(y = predicted, group = dataset), color = "#f37735", size = 1) + 
  geom_line(data = subset(data_grouped, quarantine >= 0), aes(y = predicted, group = dataset), color = "#f37735", size = 1) + 
  guides(color = guide_legend(override.aes = list(size = 1.3)))