library(tidyverse)
library(httr)
library(jsonlite)
library(text2vec)
library(plotly)
library(rio)
library(Rtsne) 
library(extrafont)
library(ggforce)
library(stringr)
library(ggrepel)

setwd("C:/Users/Victor/OneDrive - Aarhus universitet/Kodning/Python/PycharmProjects/DS_scraping_pushshift")
df <- import("C:/Users/Victor/OneDrive - Aarhus universitet/Kodning/Python/PycharmProjects/DS_scraping_pushshift/theDonaldComments.csv")
#Removing deleted and removed comments, since they will naturally cluster without any substantive meaning

`%nin%` = Negate(`%in%`)

df <- df %>% 
  filter(body %nin% c("[deleted]", "[removed]")) 

#sample 10000 comments between 1 month before the quarantine and 1 month after

set.seed(2599)
dfq1 <- df[sample(nrow(df), 100000),]

#Prepare API call
Sys.setenv(
  OPENAI_API_KEY = 'not a real key'
)

base_url <- "https://api.openai.com/v1/embeddings"
headers <- c(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")), 
             `Content-Type` = "application/json")
body <- list()

# setting model for API call:
body[["model"]] <- "text-embedding-ada-002"

input <- dfq1$body
  
body["input"] <- input
  
response <- POST(url = base_url, 
                   add_headers(.headers = headers), 
                   body = body, encode = "json")
  
completion <- response %>% 
    content(as = "text", encoding = "UTF-8") %>% 
    fromJSON(flatten = TRUE)
  
embeddingsdf <- data.frame(t(data.frame(completion$data$embedding)))
names(embeddingsdf) <- paste("dim", 1:1536, sep = "")
rownames(embeddingsdf) <- NULL
  
thedonaldbatch <- cbind(dfq1, embeddingsdf)  


set.seed(7523)
tsne_result <- Rtsne(thedonaldbatch %>% select(starts_with("dim")), dims = 2,
                     perplexity = 20, check_duplicates = FALSE)

# add t-SNE coordinates to dataframe:
thedonaldbatch[,c("tsne1", "tsne2")] <- data.frame(X = tsne_result$Y[, 1], Y = tsne_result$Y[, 2])

write.csv(thedonaldbatch, "C:/Users/Victor/OneDrive - Aarhus universitet/Kodning/Python/PycharmProjects/DS_scraping_pushshift/the_donaldsample_tsneresults.csv")
thedonaldbatch <- import("C:/Users/Victor/OneDrive - Aarhus universitet/Kodning/Python/PycharmProjects/DS_scraping_pushshift/the_donaldsample_tsneresults.csv")

df <- data.frame(tsne1 = thedonaldbatch$tsne1, tsne2 = thedonaldbatch$tsne2)


k <- 20
set.seed(2231)
clusters <- kmeans(df,centers = k)
thedonaldbatch$cluster <- clusters$cluster

#Interactive plot to label clusters
my_colors <- c('#e6194b', '#3cb44b', '#ffe119',
               '#4363d8', '#f58231', '#911eb4',
               '#46f0f0', '#f032e6', '#bcf60c', '#fabebe',
               '#008080', '#e6beff', '#9a6324', '#fffac8',
               '#800000', '#aaffc3', '#808000', '#ffd8b1',
               '#000075', '#808080', '#ffffff', '#000000')


thedonaldbatch$clustersandtext <- paste(thedonaldbatch$body, thedonaldbatch$cluster)

plot_ly(thedonaldbatch, x = ~ tsne1, y = ~ tsne2,
             type = 'scatter', mode = 'markers', text = ~ clustersandtext, 
             alpha=0.5, 
             color= ~ cluster,
             colors=my_colors,
             hoverinfo = 'text')

png(file="tsne.png",
    width=1400, height=800)

ggplot(thedonaldbatch, aes(x = tsne1, y = tsne2, color = factor(cluster))) + 
  geom_point(alpha = 0.2) +
  scale_color_manual(values = my_colors) +
  labs(x = "t-SNE Component 1", y = "t-SNE Component 2", color = "Cluster") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_text(size=30, color="black"),
        axis.text = element_text(size = 30, color = "black"))+
  guides(color="none")
dev.off()


clusterlabels <- c("Reddit censorship, deep state", "MAGA, mixed politics", "Arguing against the left", 
                   "Mixed politics", "Mixed politics", "Presidential candidates, voting, misinformation",
                   "Criticizing political opposition", "Trump, Russian interference", "Elite influence on politics, arguing with trolls",
                   "Immigrants, muslims, LGBT, women", "Communists, calling out people who call Trump nazi",
                   "Mixed politics", "The far left as fascists", "Islam, lack of mods on patriots.win",
                   "Fake news, mixed politics", "Mexico, the left", "Criticizing leftist arguments, mixed politics",
                   "Failures of democrats, mixed politics", "Unclear", "Reddit interventions, criticizing the left")


#Putting labels to the right clusters and putting the center to plot big dots, for easier visualization
for (i in 1:20)
{
 thedonaldbatch$labels[thedonaldbatch$cluster==i] <- clusterlabels[i] 
 thedonaldbatch$tsne1center[thedonaldbatch$cluster==i] <- clusters$centers[i,1]
 thedonaldbatch$tsne2center[thedonaldbatch$cluster==i] <- clusters$centers[i,2]
 thedonaldbatch$clusterfrequency[thedonaldbatch$cluster==i] <- table(thedonaldbatch$cluster)[[i]]
}
thedonaldbatch$labelsnumbers <- paste(thedonaldbatch$labels, "n=", thedonaldbatch$clusterfrequency)

plottingdata <- data.frame(clusterfrequency=thedonaldbatch$clusterfrequency, cluster= thedonaldbatch$cluster,
                           tsne1center = thedonaldbatch$tsne1center, tsne2center=thedonaldbatch$tsne2center,
                           clusterlabels = thedonaldbatch$labelsnumbers)

plottingdata <- plottingdata[!duplicated(plottingdata$cluster),]
max_width <- 20

png(file="tsne_centers.png",
    width=1900, height=1200)

plottingdata %>% 
  ggplot(aes(tsne1center, tsne2center)) +
  geom_point(aes(size = clusterfrequency, color = cluster), alpha=0.4)+
  theme_minimal()+
  scale_size_continuous(range = c(2, 30))+
  labs(x = "t-SNE Dimension 1", y = "t-SNE Dimension 2", size = 16, color = "black") +
  geom_label_repel(aes(fill=cluster, fontface="bold", label = str_wrap(clusterlabels, width = max_width)),
             size = 8, nudge_y = -0.2,
            color="white", alpha=0.2,   label.size = NA, max.overlaps=20) +
  geom_label_repel(aes(label=str_wrap(clusterlabels, width = max_width)), fill=NA, nudge_y = -0.2, size=8, label.size=NA,
                   fontface="bold", color="black",max.overlaps=20)+
  guides(color = "none", size="none", alpha="none", fill="none")+
  scale_color_gradient(low = "#ff00ff", high = "#339999") +
  scale_fill_gradient(low = "#ff00ff", high = "#339999") +
  theme(legend.position = "none", 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.title=element_text(size=40, color="black"),
        axis.ticks = element_blank(),
        axis.text = element_text(size=40, color="black"))
dev.off()


