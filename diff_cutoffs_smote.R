### Stanford University ###
### IR Honors Thesis ###
##### 2023-2024 #####
#### Irmak Ersoz #### 

# Models with SMOTE & Skewed Cutoff: Replication Code


# Load libraries 
library(caret)
library(dplyr)
library(remotes)
library(doMC)

# The below code installs remotes from cran repository
# remotes::install_github("cran/DMwR")

# Use multiple cores for parallel processing
registerDoMC(cores = 5)

# Data transformation for binary classification
ppd_only_skew <- ppd_only %>%
  mutate(bi_overall_rating_skew = ifelse(six_overall_rating <= 4, "Unsuccessful", "Successful"))
ppd_wgi_skew <- ppd_wgi %>%
  mutate(bi_overall_rating_skew = ifelse(six_overall_rating <= 4, "Unsuccessful", "Successful"))
ppd_cpia_skew <- ppd_cpia %>%
  mutate(bi_overall_rating_skew = ifelse(six_overall_rating <= 4, "Unsuccessful", "Successful"))


ppd_only_skew$bi_overall_rating_skew <- factor(ppd_only_skew$bi_overall_rating_skew)
ppd_wgi_skew$bi_overall_rating_skew <- factor(ppd_wgi_skew$bi_overall_rating_skew)
ppd_cpia_skew$bi_overall_rating_skew <- factor(ppd_cpia_skew$bi_overall_rating_skew)

# Preprocessing and model training for ppd_only (no governance measures)
set.seed(123)
ppd_only_skew_predictor_matrix_bi <- select(ppd_only_skew, -c(project_id,
                                                         project_name,
                                                         bi_overall_rating,
                                                         six_overall_rating,
                                                         multi_overall_rating))
index <- createDataPartition(ppd_only_skew$bi_overall_rating_skew, p = 0.8, list = FALSE)
train_data <- ppd_only_skew_predictor_matrix_bi[index,]
test_data <- ppd_only_skew_predictor_matrix_bi[-index,]

trC <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = defaultSummary
)

tuneGrid <- expand.grid(
  mtry = c(2, floor(sqrt(ncol(train_data) - 1)), ncol(train_data) - 1),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)


ppd_only_skew_ranger_bi <- caret::train(
  bi_overall_rating_skew ~ .,
  data = train_data,
  method = "ranger",
  trControl = trC,
  tuneGrid = tuneGrid,
  importance = "impurity",
  verbose = TRUE,
  metric = "Accuracy"
)

print(ppd_only_skew_ranger_bi)
ppd_only_bi_train_preds <- predict(ppd_only_skew_ranger_bi, newdata = train_data)
cm_train <- confusionMatrix(ppd_only_bi_train_preds, train_data$bi_overall_rating_skew)
print(cm_train)

precision_train <- cm_train$byClass['Pos Pred Value']
recall_train <- cm_train$byClass['Sensitivity']

train_true_positive_rate <- cm_train$byClass['Sensitivity']
train_true_negative_rate <- cm_train$byClass['Specificity']

print(precision_train)
print(recall_train)
print(train_true_positive_rate)
print(train_true_negative_rate)

ppd_only_bi_predictions <- predict(ppd_only_skew_ranger_bi, newdata = test_data)
cm_test <- confusionMatrix(ppd_only_bi_predictions, test_data$bi_overall_rating_skew)
print(cm_test)

precision_test <- cm_test$byClass['Pos Pred Value']
recall_test <- cm_test$byClass['Sensitivity']

print(precision_test)
print(recall_test)

test_true_positive_rate <- cm_test$byClass['Sensitivity']
test_true_negative_rate <- cm_test$byClass['Specificity']

print(paste("Test True Positive Rate: ", test_true_positive_rate))
print(paste("Test True Negative Rate: ", test_true_negative_rate))


prob_predictions <- predict(ppd_only_skew_ranger_bi, newdata = test_data, type = "prob")
roc_curve <- roc(test_data$bi_overall_rating, prob_predictions$Successful)
plot(roc_curve, main = "ROC Curve for Baseline Model")
auc_value <- auc(roc_curve)
print(auc_value)

ppd_only_skew_varimp <- varImp(ppd_only_skew_ranger_bi, scale = FALSE)
print(ppd_only_skew_varimp)

ppd_only_skew_varimp <- ppd_only_skew_varimp$importance

varimp_sorted <- ppd_only_skew_varimp %>% arrange(desc(Overall))
top10_vars <- head(varimp_sorted, 10)
top10_vars$Variable <- row.names(top10_vars)

ggplot(top10_vars, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "darkcyan") +
  coord_flip() +  # Flip the coordinates to make it a horizontal bar chart
  labs(x = "Variable", y = "Importance", 
       title = "Top 10 Variable Importances - Baseline Model with Skewed Cutoff") +
  theme_minimal()


###############################################################################

# Preprocessing and model training for ppd_wgi (with WGI)
set.seed(123)
ppd_wgi_skew_predictor_matrix_bi <- select(ppd_wgi_skew, -c(project_id,
                                                            project_name,
                                                            bi_overall_rating,
                                                            six_overall_rating,
                                                            multi_overall_rating))
index <- createDataPartition(ppd_wgi_skew$bi_overall_rating_skew, p = 0.8, list = FALSE)
train_data <- ppd_wgi_skew_predictor_matrix_bi[index,]
test_data <- ppd_wgi_skew_predictor_matrix_bi[-index,]

trC <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = defaultSummary
)

tuneGrid <- expand.grid(
  mtry = c(2, floor(sqrt(ncol(train_data) - 1)), ncol(train_data) - 1),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)

ppd_wgi_skew_ranger_bi <- caret::train(
  bi_overall_rating_skew ~ .,
  data = train_data,
  method = "ranger",
  trControl = trC,
  tuneGrid = tuneGrid,
  importance = "impurity",
  verbose = TRUE,
  metric = "Accuracy"
)

print(ppd_wgi_skew_ranger_bi)
ppd_wgi_bi_train_preds <- predict(ppd_wgi_skew_ranger_bi, newdata = train_data)
cm_train <- confusionMatrix(ppd_wgi_bi_train_preds, train_data$bi_overall_rating_skew)
print(cm_train)

precision_train <- cm_train$byClass['Pos Pred Value']
recall_train <- cm_train$byClass['Sensitivity']

train_true_positive_rate <- cm_train$byClass['Sensitivity']
train_true_negative_rate <- cm_train$byClass['Specificity']

print(precision_train)
print(recall_train)
print(train_true_positive_rate)
print(train_true_negative_rate)

ppd_wgi_bi_predictions <- predict(ppd_wgi_skew_ranger_bi, newdata = test_data)
cm_test <- confusionMatrix(ppd_wgi_bi_predictions, test_data$bi_overall_rating_skew)
print(cm_test)

precision_test <- cm_test$byClass['Pos Pred Value']
recall_test <- cm_test$byClass['Sensitivity']

print(precision_test)
print(recall_test)

test_true_positive_rate <- cm_test$byClass['Sensitivity']
test_true_negative_rate <- cm_test$byClass['Specificity']

print(paste("Test True Positive Rate: ", test_true_positive_rate))
print(paste("Test True Negative Rate: ", test_true_negative_rate))

prob_predictions <- predict(ppd_wgi_skew_ranger_bi, newdata = test_data, type = "prob")
roc_curve <- roc(test_data$bi_overall_rating_skew, prob_predictions$Successful)
plot(roc_curve, main = "ROC Curve for WGI Model with Skewed Cutoff")
auc_value <- auc(roc_curve)
print(auc_value)

ppd_wgi_skew_varimp <- varImp(ppd_wgi_skew_ranger_bi, scale = FALSE)
print(ppd_wgi_skew_varimp)

ppd_wgi_skew_varimp <- ppd_wgi_skew_varimp$importance

varimp_sorted <- ppd_wgi_skew_varimp %>% arrange(desc(Overall))
top10_vars <- head(varimp_sorted, 10)
top10_vars$Variable <- row.names(top10_vars)

ggplot(top10_vars, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "darkcyan") +
  coord_flip() +  # Flip the coordinates to make it a horizontal bar chart
  labs(x = "Variable", y = "Importance", 
       title = "Top 10 Variable Importances - WGI Model with Skewed Cutoff") +
  theme_minimal()


################################################################################
# Preprocessing and model training for ppd_cpia (with CPIA)
set.seed(123)
ppd_cpia_skew_predictor_matrix_bi <- select(ppd_cpia_skew, -c(project_id,
                                                              project_name,
                                                              bi_overall_rating,
                                                              six_overall_rating,
                                                              multi_overall_rating))
index <- createDataPartition(ppd_cpia_skew$bi_overall_rating_skew, p = 0.8, list = FALSE)
train_data <- ppd_cpia_skew_predictor_matrix_bi[index,]
test_data <- ppd_cpia_skew_predictor_matrix_bi[-index,]

trC <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = defaultSummary
)

tuneGrid <- expand.grid(
  mtry = c(2, floor(sqrt(ncol(train_data) - 1)), ncol(train_data) - 1),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)

ppd_cpia_skew_ranger_bi <- caret::train(
  bi_overall_rating_skew ~ .,
  data = train_data,
  method = "ranger",
  trControl = trC,
  tuneGrid = tuneGrid,
  importance = "impurity",
  verbose = TRUE,
  metric = "Accuracy"
)

print(ppd_cpia_skew_ranger_bi)
ppd_cpia_bi_train_preds <- predict(ppd_cpia_skew_ranger_bi, newdata = train_data)
cm_train <- confusionMatrix(ppd_cpia_bi_train_preds, train_data$bi_overall_rating_skew)
print(cm_train)

precision_train <- cm_train$byClass['Pos Pred Value']
recall_train <- cm_train$byClass['Sensitivity']

train_true_positive_rate <- cm_train$byClass['Sensitivity']
train_true_negative_rate <- cm_train$byClass['Specificity']

print(precision_train)
print(recall_train)
print(train_true_positive_rate)
print(train_true_negative_rate)

ppd_cpia_bi_predictions <- predict(ppd_cpia_skew_ranger_bi, newdata = test_data)
cm_test <- confusionMatrix(ppd_cpia_bi_predictions, test_data$bi_overall_rating_skew)
print(cm_test)

precision_test <- cm_test$byClass['Pos Pred Value']
recall_test <- cm_test$byClass['Sensitivity']

print(precision_test)
print(recall_test)

test_true_positive_rate <- cm_test$byClass['Sensitivity']
test_true_negative_rate <- cm_test$byClass['Specificity']

print(paste("Test True Positive Rate: ", test_true_positive_rate))
print(paste("Test True Negative Rate: ", test_true_negative_rate))

prob_predictions <- predict(ppd_cpia_skew_ranger_bi, newdata = test_data, type = "prob")
roc_curve <- roc(test_data$bi_overall_rating_skew, prob_predictions$Successful)
plot(roc_curve, main = "ROC Curve for CPIA Model with Skewed Cutoff")
auc_value <- auc(roc_curve)
print(auc_value)

ppd_cpia_skew_varimp <- varImp(ppd_cpia_skew_ranger_bi, scale = FALSE)
print(ppd_cpia_skew_varimp)

ppd_cpia_skew_varimp <- ppd_cpia_skew_varimp$importance

varimp_sorted <- ppd_cpia_skew_varimp %>% arrange(desc(Overall))
top10_vars <- head(varimp_sorted, 10)
top10_vars$Variable <- row.names(top10_vars)

ggplot(top10_vars, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "darkcyan") +
  coord_flip() +  # Flip the coordinates to make it a horizontal bar chart
  labs(x = "Variable", y = "Importance", 
       title = "Top 10 Variable Importances - CPIA Model with Skewed Cutoff") +
  theme_minimal()

