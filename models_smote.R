### Stanford University ###
### IR Honors Thesis ###
##### 2023-2024 #####
#### Irmak Ersoz #### 

# Models without skewed cutoff: Replication Code

# Load necessary packages
library(caret)
library(DMwR)
library(dplyr)
library(remotes)
library(doMC)

# Below code installs remotes from cran repository
# remotes::install_github("cran/DMwR")

# Use multiple cores for parallel processing
registerDoMC(cores = 5)

# SMOTE SAMPLING WITH RANGER

# Preprocessing and model training for ppd_only (no governance measures)
set.seed(123)
ppd_only_predictor_matrix_bi <- select(ppd_only, -c(project_id,
                                                    project_name,
                                                    six_overall_rating,
                                                    multi_overall_rating))
index <- createDataPartition(ppd_only_predictor_matrix_bi$bi_overall_rating, 
                             p = 0.8, list = FALSE)
train_data_pre <- ppd_only_predictor_matrix_bi[index,]
test_data <- ppd_only_predictor_matrix_bi[-index,]
train_data_balanced <- SMOTE(bi_overall_rating ~ ., data = train_data_pre, 
                             perc.over = 100, k = 5, method = "SMOTE-NC")
baseline_smote <- train_data_balanced # for plotting

trC <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = defaultSummary  # Uses Accuracy, Kappa etc.
)

tuneGrid <- expand.grid(
  mtry = c(2, floor(sqrt(ncol(train_data_balanced) - 1)), 
           ncol(train_data_balanced) - 1), 
  splitrule = c("gini", "extratrees"), 
  min.node.size = c(1, 5, 10)
)

levels(train_data_balanced$bi_overall_rating) <- make.names(levels(train_data_balanced$bi_overall_rating))

levels(train_data_balanced$bi_overall_rating) <- c("Unsuccessful", "Successful")

ppd_only_ranger_bi <- caret::train(
  bi_overall_rating ~ .,
  data = train_data_balanced,
  method = "ranger",
  trControl = trC,
  tuneGrid = tuneGrid,
  importance = "impurity",
  verbose = TRUE,
  metric = "Accuracy"
)

print(ppd_only_ranger_bi)
ppd_only_bi_train_preds <- predict(ppd_only_ranger_bi, newdata = train_data_balanced)

# Make confusion matrix and extract precision & recall
cm_train <- confusionMatrix(ppd_only_bi_train_preds, train_data_balanced$bi_overall_rating)
print(cm_train)
precision_train <- cm_train$byClass['Pos Pred Value']
recall_train <- cm_train$byClass['Sensitivity']


# Calculate True Positive Rate (TPR) and True Negative Rate (TNR) for the training set
train_true_positive_rate <- cm_train$byClass['Sensitivity']
train_true_negative_rate <- cm_train$byClass['Specificity']

print(precision_train)
print(recall_train)
print(train_true_positive_rate)
print(train_true_negative_rate)


# Apply the same factor level renaming to the test data
levels(test_data$bi_overall_rating) <- c("Unsuccessful", "Successful")
ppd_only_bi_predictions <- predict(ppd_only_ranger_bi, newdata = test_data)

# Generate confusion matrix
cm_test <- confusionMatrix(ppd_only_bi_predictions, test_data$bi_overall_rating)
print(cm_test)

# Extract and plot variable importance
ppd_only_bi_varimp <- varImp(ppd_only_ranger_bi, scale = FALSE)
print(ppd_only_bi_varimp)

ppd_only_bi_varimp <- ppd_only_bi_varimp$importance

varimp_sorted <- ppd_only_bi_varimp %>% arrange(desc(Overall))
top10_vars <- head(varimp_sorted, 10)
top10_vars$Variable <- row.names(top10_vars)

ggplot(top10_vars, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "darkcyan") +
  coord_flip() +  # Flip the coordinates to make it a horizontal bar chart
  labs(x = "Variable", y = "Importance", 
       title = "Top 10 Variable Importances - Baseline Model") +
  theme_minimal()

# Calculate precision and recall
precision_test <- cm_test$byClass['Pos Pred Value']
recall_test <- cm_test$byClass['Sensitivity']

print(precision_test)
print(recall_test)

# Calculate True Positive Rate (TPR) and True Negative Rate (TNR) for the test set
test_true_positive_rate <- cm_test$byClass['Sensitivity']
test_true_negative_rate <- cm_test$byClass['Specificity']

# Print test rates
print(paste("Test True Positive Rate: ", test_true_positive_rate))
print(paste("Test True Negative Rate: ", test_true_negative_rate))


# Calculate ROC and AUC 
# Predict probabilities for ROC and AUC calculations
prob_predictions <- predict(ppd_only_ranger_bi, newdata = test_data, type = "prob")

# Calculate ROC and AUC using pROC package
if(!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
library(pROC)
roc_curve <- roc(test_data$bi_overall_rating, prob_predictions$Successful)
plot(roc_curve, main = "ROC Curve for Baseline Model")
auc_value <- auc(roc_curve)
print(auc_value)


###############################################################################

# Preprocessing and model training for ppd_wgi (with WGI)
set.seed(123)
ppd_wgi_predictor_matrix_bi <- select(ppd_wgi, -c(project_id,
                                                  project_name,
                                                  six_overall_rating,
                                                  multi_overall_rating))
index <- createDataPartition(ppd_wgi_predictor_matrix_bi$bi_overall_rating, 
                             p = 0.8, list = FALSE)
train_data_pre <- ppd_wgi_predictor_matrix_bi[index,]
test_data <- ppd_wgi_predictor_matrix_bi[-index,]
train_data_balanced <- SMOTE(bi_overall_rating ~ ., data = train_data_pre, 
                             perc.over = 100, k = 5, method = "SMOTE-NC")
wgi_smote <- train_data_balanced #for plotting


trC <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = defaultSummary  # Uses Accuracy, Kappa etc.
)

tuneGrid <- expand.grid(
  mtry = c(2, floor(sqrt(ncol(train_data_balanced) - 1)), ncol(train_data_balanced) - 1),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)

levels(train_data_balanced$bi_overall_rating) <- make.names(levels(train_data_balanced$bi_overall_rating))

levels(train_data_balanced$bi_overall_rating) <- c("Unsuccessful", "Successful")

ppd_wgi_ranger_bi <- caret::train(
  bi_overall_rating ~ .,
  data = train_data_balanced,
  method = "ranger",
  trControl = trC,
  tuneGrid = tuneGrid,
  importance = "impurity",
  verbose = TRUE,
  metric = "Accuracy"
)

print(ppd_wgi_ranger_bi)
ppd_wgi_bi_train_preds <- predict(ppd_wgi_ranger_bi, newdata = train_data_balanced)
cm_train <- confusionMatrix(ppd_wgi_bi_train_preds, train_data_balanced$bi_overall_rating)
print(cm_train)

precision_train <- cm_train$byClass['Pos Pred Value']
recall_train <- cm_train$byClass['Sensitivity']


# Calculate True Positive Rate (TPR) and True Negative Rate (TNR) for the training set
train_true_positive_rate <- cm_train$byClass['Sensitivity']
train_true_negative_rate <- cm_train$byClass['Specificity']

print(precision_train)
print(recall_train)
print(train_true_positive_rate)
print(train_true_negative_rate)


levels(test_data$bi_overall_rating) <- c("Unsuccessful", "Successful")
ppd_wgi_bi_predictions <- predict(ppd_wgi_ranger_bi, newdata = test_data)
cm_test <- confusionMatrix(ppd_wgi_bi_predictions, test_data$bi_overall_rating)
print(cm_test)

# Extract and plot variable importance
ppd_wgi_bi_varimp <- varImp(ppd_wgi_ranger_bi, scale = FALSE)
print(ppd_wgi_bi_varimp)

ppd_wgi_bi_varimp <- ppd_wgi_bi_varimp$importance

varimp_sorted <- ppd_wgi_bi_varimp %>% arrange(desc(Overall))
top10_vars <- head(varimp_sorted, 10)
top10_vars$Variable <- row.names(top10_vars)

ggplot(top10_vars, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "darkcyan") +
  coord_flip() +  # Flip the coordinates to make it a horizontal bar chart
  labs(x = "Variable", y = "Importance", 
       title = "Top 10 Variable Importances - WGI Model") +
  theme_minimal()

# Calculate precision and recall
precision_test <- cm_test$byClass['Pos Pred Value']
recall_test <- cm_test$byClass['Sensitivity']

print(precision_test)
print(recall_test)

# Calculate True Positive Rate (TPR) and True Negative Rate (TNR) for the test set
test_true_positive_rate <- cm_test$byClass['Sensitivity']
test_true_negative_rate <- cm_test$byClass['Specificity']

# Print test rates
print(paste("Test True Positive Rate: ", test_true_positive_rate))
print(paste("Test True Negative Rate: ", test_true_negative_rate))


# Print training rates
print(paste("Training True Positive Rate: ", train_true_positive_rate))
print(paste("Training True Negative Rate: ", train_true_negative_rate))


# Calculate ROC and AUC 
# Predict probabilities for ROC and AUC calculations
prob_predictions <- predict(ppd_wgi_ranger_bi, newdata = test_data, type = "prob")

# Calculate ROC and AUC using pROC package
if(!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
library(pROC)
roc_curve <- roc(test_data$bi_overall_rating, prob_predictions$Successful)
plot(roc_curve, main = "ROC Curve for WGI Model")
auc_value <- auc(roc_curve)
print(auc_value)


################################################################################


# Preprocessing and model training for ppd_cpia (with CPIA)
set.seed(123)
ppd_cpia_predictor_matrix_bi <- select(ppd_cpia, -c(project_id,
                                                    project_name,
                                                    six_overall_rating,
                                                    multi_overall_rating))
index <- createDataPartition(ppd_cpia_predictor_matrix_bi$bi_overall_rating, 
                             p = 0.8, list = FALSE)
train_data_pre <- ppd_cpia_predictor_matrix_bi[index,]
test_data <- ppd_cpia_predictor_matrix_bi[-index,]
train_data_balanced <- SMOTE(bi_overall_rating ~ ., data = train_data_pre, 
                             perc.over = 100, k = 5, method = "SMOTE-NC")
cpia_smote <- train_data_balanced # for plotting

trC <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = defaultSummary  # Uses Accuracy, Kappa etc.
)

tuneGrid <- expand.grid(
  mtry = c(2, floor(sqrt(ncol(train_data_balanced) - 1)), ncol(train_data_balanced) - 1),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)

levels(train_data_balanced$bi_overall_rating) <- make.names(levels(train_data_balanced$bi_overall_rating))

levels(train_data_balanced$bi_overall_rating) <- c("Unsuccessful", "Successful")

ppd_cpia_ranger_bi <- caret::train(
  bi_overall_rating ~ .,
  data = train_data_balanced,
  method = "ranger",
  trControl = trC,
  tuneGrid = tuneGrid,
  importance = "impurity",
  verbose = TRUE,
  metric = "Accuracy"
)

print(ppd_cpia_ranger_bi)
ppd_cpia_bi_train_preds <- predict(ppd_cpia_ranger_bi, newdata = train_data_balanced)
cm_train <- confusionMatrix(ppd_cpia_bi_train_preds, train_data_balanced$bi_overall_rating)
print(cm_train)

precision_train <- cm_train$byClass['Pos Pred Value']
recall_train <- cm_train$byClass['Sensitivity']


# Calculate True Positive Rate (TPR) and True Negative Rate (TNR) for the training set
train_true_positive_rate <- cm_train$byClass['Sensitivity']
train_true_negative_rate <- cm_train$byClass['Specificity']

print(precision_train)
print(recall_train)
print(train_true_positive_rate)
print(train_true_negative_rate)


levels(test_data$bi_overall_rating) <- c("Unsuccessful", "Successful")
ppd_cpia_bi_predictions <- predict(ppd_cpia_ranger_bi, newdata = test_data)
cm_test <- confusionMatrix(ppd_cpia_bi_predictions, test_data$bi_overall_rating)
print(cm_test)

# Extract and plot variable importance
ppd_cpia_bi_varimp <- varImp(ppd_cpia_ranger_bi, scale = FALSE)
print(ppd_cpia_bi_varimp)

ppd_cpia_bi_varimp <- ppd_cpia_bi_varimp$importance

varimp_sorted <- ppd_cpia_bi_varimp %>% arrange(desc(Overall))
top10_vars <- head(varimp_sorted, 10)
top10_vars$Variable <- row.names(top10_vars)

ggplot(top10_vars, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "darkcyan") +
  coord_flip() +  # Flip the coordinates to make it a horizontal bar chart
  labs(x = "Variable", y = "Importance", 
       title = "Top 10 Variable Importances - CPIA Model") +
  theme_minimal()

# Calculate precision and recall
precision_test <- cm_test$byClass['Pos Pred Value']
recall_test <- cm_test$byClass['Sensitivity']

print(precision_test)
print(recall_test)

# Calculate True Positive Rate (TPR) and True Negative Rate (TNR) for the test set
test_true_positive_rate <- cm_test$byClass['Sensitivity']
test_true_negative_rate <- cm_test$byClass['Specificity']

# Print test rates
print(paste("Test True Positive Rate: ", test_true_positive_rate))
print(paste("Test True Negative Rate: ", test_true_negative_rate))


# Print training rates
print(paste("Training True Positive Rate: ", train_true_positive_rate))
print(paste("Training True Negative Rate: ", train_true_negative_rate))


# Calculate ROC and AUC 
# Predict probabilities for ROC and AUC calculations
prob_predictions <- predict(ppd_cpia_ranger_bi, newdata = test_data, type = "prob")

# Calculate ROC and AUC
roc_curve <- roc(test_data$bi_overall_rating, prob_predictions$Successful)
plot(roc_curve, main = "ROC Curve for CPIA Model")
auc_value <- auc(roc_curve)
print(auc_value)


################################################################################

# Plot comparing model accuracies
# Create a data frame with the model accuracies
data <- data.frame(
  Model = rep(c("Baseline (no metrics)", "Model with WGI", "Model with CPIA"), each = 2),
  Accuracy_Type = rep(c("Training", "Testing"), 3),
  Accuracy = c(78.36, 67.77, 84.08, 75.47, 87.54, 77.47)
)

# Ensure the order of Accuracy_Type so that Training bars appear before Testing
data$Accuracy_Type <- factor(data$Accuracy_Type, levels = c("Training", "Testing"))

# Plotting the bar chart
ggplot(data, aes(fill = Accuracy_Type, y = Accuracy, x = Model)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = sprintf("%.2f%%", Accuracy), 
                y = Accuracy + 1.5),   # Adjust y position for label visibility
            position = position_dodge(width = 0.9), vjust = -0.25) +
  scale_fill_manual(values = c("Training" = "lightcyan", "Testing" = "darkcyan")) +
  labs(title = "Comparing Model Accuracies to Understand the Impact of Governance Measures",
       x = "Model Type",
       y = "Accuracy (%)",
       fill = "Accuracy Type") +
  theme_minimal()






