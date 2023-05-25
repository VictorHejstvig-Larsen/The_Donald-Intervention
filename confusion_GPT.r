library(rio)
library(irr)
library(tidyverse)
library(caret)
setwd("")

patriots <- import("")
commondata <- import("")

commondata$V1 <- NULL
commondata$dataset <- NULL
commondata$username <- NULL
commondata$comment <- NULL
commondata$GPThate <- NULL
patriots$Column1 <- NULL
patriots$comment <- NULL
commondata$id <- NULL

colnames(commondata) <- c("GPT", "human")
colnames(patriots) <- c("GPT", "human")

commondata <- rbind(commondata,patriots)
commondata[commondata$GPT=="NA",] <- 0

#Confusionmatrix
cm <- confusionMatrix(as.factor(commondata$human), as.factor(commondata$GPT))
print(cm)


confusion_matrix_df <- as.data.frame(cm$table)

confusion_matrix_df$Freq <- round(confusion_matrix_df$Freq/sum(confusion_matrix_df$Freq),
                                  digits = 2)

confusion_matrix_plot <- ggplot(data = confusion_matrix_df, 
                                aes(x = Reference, y = reorder(Prediction, desc(Prediction)), fill = Freq
                                 )) +
  geom_tile(alpha=1, color="black") +
  geom_text(aes(label = Freq), color = "black", fontface = "bold") +
  scale_fill_gradient2(low = "#dccbb7", high = "#84abb3", midpoint = max(confusion_matrix_df$Freq) / 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "True Labels", y = "Predicted Labels", fill = "Frequency")+
  guides(fill="none")+
  annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = 2.5, 
           label = paste0())+
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "white"))

print(confusion_matrix_plot)

#Sampling 20 random disagreements

patriots <- import("")
commondata <- import("")

commondata$V1 <- NULL
commondata$dataset <- NULL
commondata$username <- NULL
commondata$GPThate <- NULL
patriots$Column1 <- NULL
commondata$id <- NULL

colnames(commondata) <- c("comments","GPT", "human")
colnames(patriots) <- colnames(commondata)
patriots$GPT <- as.numeric(patriots$GPT)

commondata <- rbind(commondata,patriots)

disagreements <- commondata[commondata$human !=commondata$GPT,]

sample_dis <- disagreements[sample(nrow(disagreements), 20),] 

write.csv(sample_dis, "sample_dis.csv")
