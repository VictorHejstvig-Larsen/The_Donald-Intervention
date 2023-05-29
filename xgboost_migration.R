library(tidyverse)
library(mlr3)
library(mlr3learners)
library(rpart.plot)
library(mlr3tuning)
library(xgboost)
library(rio)
library(caret)
library(R.filesets)

setwd("C:/Users/Victor/OneDrive/TD_data")

df <- import("migration_prediction_dataset.csv")

df <- df %>% 
  select(-contains("username"))

df$migration <- as.factor(df$migration)

#XGBOOST
set.seed(10102)
training_rows <- sample(nrow(df), 
                        size = round(0.8*nrow(df)), replace = F)
migration_training <- df %>% dplyr::slice(training_rows)
migration_test <- df %>% dplyr::slice(-(training_rows))

task_migration <- TaskClassif$new(id = "id", backend = migration_training, 
                                    target = "migration")

learner_xgboost <- lrn("classif.xgboost", 
                       eta = to_tune(0, 1),
                       gamma = to_tune(0, 5),
                       "max_depth" = to_tune(1, 5),
                       "min_child_weight" = to_tune(1, 10),
                       "subsample" = to_tune(0.5, 1),
                       "colsample_bytree" = to_tune(0.5, 1),
                       "nrounds" = to_tune(10, 20))

instance <- ti(
  task = task_migration,
  learner = learner_xgboost,
  resampling = rsmp("cv", folds = 10),
  measures = msr("classif.acc"),
  terminator = trm("none")
)

tuner <- tnr("grid_search", resolution = 2, batch_size = 10)


set.seed(2983)
tuner$optimize(instance)

# optimal result:
instance$result

# use tuned hyperparameter values to create final model:

learner_xgboost_tuned <- lrn("classif.xgboost")
learner_xgboost_tuned$param_set$values <- instance$result_learner_param_vals
learner_xgboost_tuned$train(task_migration)

task_migration_test <- TaskClassif$new(id = "id", 
                                     backend = migration_test, 
                                     target = "migration")

predictions <- learner_xgboost_tuned$predict(task_migration_test)


#Results
confusionMatrix(predictions$response, predictions$truth)

##### Plot confusion matrix for xgboost -----

conf_matrix_xgboost <- confusionMatrix(predictions$response, predictions$truth)

confusion_matrix_df <- as.data.frame(conf_matrix_xgboost$table)

confusion_matrix_df$Freq <- round(confusion_matrix_df$Freq/sum(confusion_matrix_df$Freq),
                                  digits = 2)

confusion_matrix_plot <- ggplot(data = confusion_matrix_df, 
                                aes(x = Reference, y = reorder(Prediction, desc(Prediction)), fill = Freq)) +
  geom_tile(alpha=0.8, color="black") +
  geom_text(aes(label = Freq), color = "black", fontface = "bold") +
  scale_fill_gradient2(low = "#dccbb7", high = "#84abb3", midpoint = max(confusion_matrix_df$Freq) / 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "True Labels", y = "Predicted Labels", fill = "Frequency", title = "XGboost")+
  guides(fill="none")+
  annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = 2.5, 
           label = paste0("               Accuracy = ", 0.97, "\n",
                          "No information rate = ", 0.97))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "white"))

print(confusion_matrix_plot)

