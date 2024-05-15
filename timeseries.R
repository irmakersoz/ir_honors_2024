### Stanford University ###
### IR Honors Thesis ###
##### 2023-2024 #####
#### Irmak Ersoz #### 

# Time Series Testing: Replication Code


# Time series testing without skewed cutoff
# No governance metrics
# Load necessary libraries
library(caret)
library(DMwR)  # For SMOTE
library(dplyr) # For data manipulation
library(ranger)
library(doMC)

# Use multiple cores
registerDoMC(cores = 5)

# Set seed for reproducibility
set.seed(123)


# Define the cutoff year 
cutoff_year <- 2014

# Create training and test datasets
train_data_pre <- ppd_only[which(ppd_only$start_year < cutoff_year), ]
train_data_pre <- train_data_pre %>% select(-c("six_overall_rating", 
                                               "multi_overall_rating",
                                               "project_id",
                                               "project_name"))
test_data_only <- ppd_only[which(ppd_only$start_year >= cutoff_year), ]
test_data_only <- test_data_only %>% select(-c("six_overall_rating",
                                     "multi_overall_rating",
                                     "project_id",
                                     "project_name"))


# Applying SMOTE to training data only
train_data_balanced <- SMOTE(bi_overall_rating ~ ., data = train_data_pre, perc.over = 100, k = 5, method = "SMOTE-NC")
levels(train_data_balanced$bi_overall_rating) <- make.names(levels(train_data_balanced$bi_overall_rating), unique = TRUE)

# Specify tuning grid and control parameters
trC <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = defaultSummary  
)

tuneGrid <- expand.grid(
  mtry = c(2, floor(sqrt(ncol(train_data_balanced) - 1)), ncol(train_data_balanced) - 1),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)

# Training the model using ranger
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

# Predict on test data and evaluate performance
test_pred <- predict(ppd_only_ranger_bi, newdata = test_data_only)
levels(test_pred) <- c("0", "1")
confusionMatrix(test_pred, test_data_only$bi_overall_rating)

#Predict on train data
train_pred <- predict(ppd_only_ranger_bi, newdata = train_data_balanced)
levels(train_pred) <- c("X0", "X1")
confusionMatrix(train_pred, train_data_balanced$bi_overall_rating)

# Variable importance
varImp(ppd_only_ranger_bi)

# ROC curve
# Predict probabilities for ROC and AUC calculations
prob_predictions <- predict(ppd_only_ranger_bi, newdata = test_data_only, type = "prob")

# Calculate ROC and AUC using pROC package
if(!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
library(pROC)
roc_curve <- roc(test_data_only$bi_overall_rating, prob_predictions$Successful)
plot(roc_curve, main = "ROC Curve for Baseline Model")
auc_value <- auc(roc_curve)
print(auc_value)




# WGI
# Set seed for reproducibility
set.seed(123)

# Edit dataset factor labels
sapply(ppd_wgi_factor, function(x) if(is.factor(x)) levels(x))
ppd_wgi_factor[] <- lapply(ppd_wgi_factor, function(x) if(is.character(x)) factor(x) else x)
levels(ppd_wgi_factor$bi_overall_rating) <- make.names(levels(ppd_wgi_factor$bi_overall_rating), unique = TRUE)

# Define the cutoff year 
cutoff_year <- 2014

# Create training and test datasets
train_data_pre <- ppd_wgi[which(ppd_wgi$start_year < cutoff_year), ]
train_data_pre <- train_data_pre %>% select(-c("six_overall_rating", 
                                               "multi_overall_rating",
                                               "project_id",
                                               "project_name"))
test_data_wgi <- ppd_wgi[which(ppd_wgi$start_year >= cutoff_year), ]
test_data_wgi <- test_data_wgi %>% select(-c("six_overall_rating",
                                     "multi_overall_rating",
                                     "project_id",
                                     "project_name"))


# Applying SMOTE to training data only
train_data_balanced <- SMOTE(bi_overall_rating ~ ., data = train_data_pre, perc.over = 100, k = 5, method = "SMOTE-NC")
levels(train_data_balanced$bi_overall_rating) <- make.names(levels(train_data_balanced$bi_overall_rating), unique = TRUE)

# Specify tuning grid and control parameters
trC <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = defaultSummary  
)

tuneGrid <- expand.grid(
  mtry = c(2, floor(sqrt(ncol(train_data_balanced) - 1)), ncol(train_data_balanced) - 1),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)


# Training the model using ranger
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

# Predict on test data and evaluate performance
test_pred <- predict(ppd_wgi_ranger_bi, newdata = test_data_wgi)
levels(test_pred) <- c("0", "1")
confusionMatrix(test_pred, test_data_wgi$bi_overall_rating)

# Predict on train data
train_pred <- predict(ppd_wgi_ranger_bi, newdata = train_data_balanced)
levels(train_pred) <- c("X0", "X1")
confusionMatrix(train_pred, train_data_balanced$bi_overall_rating)

# Variable importance
varImp(ppd_wgi_ranger_bi)



# CPIA
# Set seed for reproducibility
set.seed(123)

# Edit dataset factor labels
sapply(ppd_cpia_factor, function(x) if(is.factor(x)) levels(x))
ppd_cpia_factor[] <- lapply(ppd_cpia_factor, function(x) if(is.character(x)) factor(x) else x)
levels(ppd_cpia_factor$bi_overall_rating) <- make.names(levels(ppd_cpia_factor$bi_overall_rating), unique = TRUE)

# Define the cutoff year 
cutoff_year <- 2014

# Create training and test datasets
train_data_pre <- ppd_cpia[which(ppd_cpia$start_year < cutoff_year), ]
train_data_pre <- train_data_pre %>% select(-c("six_overall_rating", 
                                               "multi_overall_rating",
                                               "project_id",
                                               "project_name"))
test_data_cpia <- ppd_cpia[which(ppd_cpia$start_year >= cutoff_year), ]
test_data_cpia <- test_data_cpia %>% select(-c("six_overall_rating",
                                     "multi_overall_rating",
                                     "project_id",
                                     "project_name"))


# Applying SMOTE to training data only
train_data_balanced <- SMOTE(bi_overall_rating ~ ., data = train_data_pre, perc.over = 100, k = 5, method = "SMOTE-NC")
levels(train_data_balanced$bi_overall_rating) <- make.names(levels(train_data_balanced$bi_overall_rating), unique = TRUE)

# Specify tuning grid and control parameters
trC <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = defaultSummary  
)

tuneGrid <- expand.grid(
  mtry = c(2, floor(sqrt(ncol(train_data_balanced) - 1)), ncol(train_data_balanced) - 1),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)

# Training the model using ranger
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

# Predict on test data and evaluate performance
test_pred <- predict(ppd_cpia_ranger_bi, newdata = test_data_cpia)
levels(test_pred) <- c("0", "1")
confusionMatrix(test_pred, test_data_cpia$bi_overall_rating)

# Predict on train data
train_pred <- predict(ppd_cpia_ranger_bi, newdata = train_data_balanced)
levels(train_pred) <- c("X0", "X1")
confusionMatrix(train_pred, train_data_balanced$bi_overall_rating)

# Variable importance
varImp(ppd_cpia_ranger_bi)


# Plot ROCs 
library(pROC)



# Plotting layered ROCs
# Predict probabilities for each dataset
prob_predictions_only <- predict(ppd_only_ranger_bi, newdata = test_data_only, type = "prob")
prob_predictions_wgi <- predict(ppd_wgi_ranger_bi, newdata = test_data_wgi, type = "prob")
prob_predictions_cpia <- predict(ppd_cpia_ranger_bi, newdata = test_data_cpia, type = "prob")

# Compute ROC curves
roc_curve_only <- roc(test_data_only$bi_overall_rating, prob_predictions_only$X1)
roc_curve_wgi <- roc(test_data_wgi$bi_overall_rating, prob_predictions_wgi$X1)
roc_curve_cpia <- roc(test_data_cpia$bi_overall_rating, prob_predictions_cpia$X1)

# Plot all ROC curves
plot(roc_curve_only, main = "Comparative ROC Curves, Time Series without Skewed Cutoffs", col = "cyan2")
plot(roc_curve_wgi, add = TRUE, col = "darkcyan")
plot(roc_curve_cpia, add = TRUE, col = "cyan3")

# Add a legend
legend("bottomright", legend = c("Baseline", "WGI", "CPIA"), 
       col = c("cyan2", "darkcyan", "cyan3"), lwd = 2)




#### 


# Time series testing with skewed cutoff
# Load necessary libraries

# Set a seed for reproducibility
set.seed(123)

# Define the cutoff year 
cutoff_year <- 2014

# Data transformation for binary classification
ppd_only <- ppd_only %>%
  mutate(bi_overall_rating_skew = ifelse(six_overall_rating <= 4, "Unsuccessful", "Successful")) %>%
  select(-c("six_overall_rating", "multi_overall_rating", "project_id", "project_name"))

# Convert the binary outcome to a factor
ppd_only$bi_overall_rating_skew <- factor(ppd_only$bi_overall_rating_skew)

# Split the data into training and test sets based on the year
ppd_only_skew_predictor_matrix_bi <- select(ppd_only_skew, -c(project_id,
                                                              project_name,
                                                              bi_overall_rating,
                                                              six_overall_rating,
                                                              multi_overall_rating))
train_data <- ppd_only_skew_predictor_matrix_bi[ppd_only_skew_predictor_matrix_bi$start_year < cutoff_year, ]
test_data_only <- ppd_only_skew_predictor_matrix_bi[ppd_only_skew_predictor_matrix_bi$start_year >= cutoff_year, ]

# Create trainControl for the model
trC <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = defaultSummary
)

# Define the tuning grid for the ranger method
tuneGrid <- expand.grid(
  mtry = c(2, floor(sqrt(ncol(train_data) - 1)), ncol(train_data) - 1),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)

# Train the ranger model
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

# Print the model summary
print(ppd_only_skew_ranger_bi)

# Calculate and print the confusion matrix for both train and test sets
ppd_only_bi_predictions_train <- predict(ppd_only_skew_ranger_bi, newdata = train_data)
cm_train <- confusionMatrix(ppd_only_bi_predictions_train, train_data$bi_overall_rating_skew)
print(cm_train)

ppd_only_bi_predictions_test <- predict(ppd_only_skew_ranger_bi, newdata = test_data_only)
cm_test <- confusionMatrix(ppd_only_bi_predictions_test, test_data_only$bi_overall_rating_skew)
print(cm_test)

# Calculate and print ROC curve and AUC for the test data
prob_predictions <- predict(ppd_only_skew_ranger_bi, newdata = test_data_only, type = "prob")
roc_curve_baseline_skew <- roc(test_data_only$bi_overall_rating_skew, prob_predictions$Successful)
plot(roc_curve_baseline_skew, main = "ROC Curve for Baseline Model")
auc_value <- auc(roc_curve)
print(paste("AUC Value: ", auc_value))

# Print variable importance
ppd_only_skew_varimp <- varImp(ppd_only_skew_ranger_bi, scale = FALSE)
print(ppd_only_skew_varimp)



# WGI
# Split the data into training and test sets based on the year
ppd_wgi_skew_predictor_matrix_bi <- select(ppd_wgi_skew, -c(project_id,
                                                            project_name,
                                                            bi_overall_rating,
                                                            six_overall_rating,
                                                            multi_overall_rating))
train_data <- ppd_wgi_skew_predictor_matrix_bi[ppd_wgi_skew_predictor_matrix_bi$start_year < cutoff_year, ]
test_data_wgi <- ppd_wgi_skew_predictor_matrix_bi[ppd_wgi_skew_predictor_matrix_bi$start_year >= cutoff_year, ]

# Create trainControl for the model
trC <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = defaultSummary
)

# Define the tuning grid for the ranger method
tuneGrid <- expand.grid(
  mtry = c(2, floor(sqrt(ncol(train_data) - 1)), ncol(train_data) - 1),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)

# Train the ranger model
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

# Print the model summary
print(ppd_wgi_skew_ranger_bi)

# Calculate and print the confusion matrix for both train and test sets
ppd_wgi_bi_predictions_train <- predict(ppd_wgi_skew_ranger_bi, newdata = train_data)
cm_train <- confusionMatrix(ppd_wgi_bi_predictions_train, train_data$bi_overall_rating_skew)
print(cm_train)

ppd_wgi_bi_predictions_test <- predict(ppd_wgi_skew_ranger_bi, newdata = test_data_wgi)
cm_test <- confusionMatrix(ppd_wgi_bi_predictions_test, test_data$bi_overall_rating_skew)
print(cm_test)

# Calculate and print ROC curve and AUC for the test data
prob_predictions <- predict(ppd_wgi_skew_ranger_bi, newdata = test_data_wgi, type = "prob")
roc_curve_wgi_skew <- roc(test_data_wgi$bi_overall_rating_skew, prob_predictions$Successful)
plot(roc_curve_wgi_skew, main = "ROC Curve for Model with Governance Measures")
auc_value <- auc(roc_curve_wgi_skew)
print(paste("AUC Value: ", auc_value))

# Print variable importance
ppd_wgi_skew_varimp <- varImp(ppd_wgi_skew_ranger_bi, scale = FALSE)
print(ppd_wgi_skew_varimp)



# CPIA 
ppd_cpia_skew_predictor_matrix_bi <- select(ppd_cpia_skew, -c(project_id,
                                                              project_name,
                                                              bi_overall_rating,
                                                              six_overall_rating,
                                                              multi_overall_rating))
train_data <- ppd_cpia_skew_predictor_matrix_bi[ppd_cpia_skew_predictor_matrix_bi$start_year < cutoff_year, ]
test_data_cpia <- ppd_cpia_skew_predictor_matrix_bi[ppd_cpia_skew_predictor_matrix_bi$start_year >= cutoff_year, ]

# Create trainControl for the model
trC <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = defaultSummary
)

# Define the tuning grid for the ranger method
tuneGrid <- expand.grid(
  mtry = c(2, floor(sqrt(ncol(train_data) - 1)), ncol(train_data) - 1),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)

# Train the ranger model
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

# Print the model summary
print(ppd_cpia_skew_ranger_bi)

# Calculate and print the confusion matrix for both train and test sets
ppd_cpia_bi_predictions_train <- predict(ppd_cpia_skew_ranger_bi, newdata = train_data)
cm_train <- confusionMatrix(ppd_cpia_bi_predictions_train, train_data$bi_overall_rating_skew)
print(cm_train)
test_data_cpia
ppd_cpia_bi_predictions_test <- predict(ppd_cpia_skew_ranger_bi, newdata = test_data)
cm_test <- confusionMatrix(ppd_cpia_bi_predictions_test, test_data_cpia$bi_overall_rating_skew)
print(cm_test)

# Calculate and print ROC curve and AUC for the test data
prob_predictions <- predict(ppd_cpia_skew_ranger_bi, newdata = test_data_cpia, type = "prob")
roc_curve_cpia_skew <- roc(test_data_cpia$bi_overall_rating_skew, prob_predictions$Successful)
plot(roc_curve_cpia_skew, main = "ROC Curve for Model with CPIA Measures")
auc_value <- auc(roc_curve_cpia_skew)
print(paste("AUC Value: ", auc_value))

# Print variable importance
ppd_cpia_skew_varimp <- varImp(ppd_cpia_skew_ranger_bi, scale = FALSE)
print(ppd_cpia_skew_varimp)





# Plot layered ROCs 

# Predict probabilities for each dataset
prob_predictions_only <- predict(ppd_only_skew_ranger_bi, newdata = test_data_only, type = "prob")
prob_predictions_wgi <- predict(ppd_wgi_skew_ranger_bi, newdata = test_data_wgi, type = "prob")
prob_predictions_cpia <- predict(ppd_cpia_skew_ranger_bi, newdata = test_data_cpia, type = "prob")

# Compute ROC curves
roc_curve_only <- roc(test_data_only$bi_overall_rating_skew, prob_predictions_only$Successful)
roc_curve_wgi <- roc(test_data_wgi$bi_overall_rating_skew, prob_predictions_wgi$Successful)
roc_curve_cpia <- roc(test_data_cpia$bi_overall_rating_skew, prob_predictions_cpia$Successful)

# Plot all ROC curves
plot(roc_curve_only, main = "Comparative ROC Curves, Time Series with Skewed Cutoffs", col = "cyan2")
plot(roc_curve_wgi, add = TRUE, col = "darkcyan")
plot(roc_curve_cpia, add = TRUE, col = "cyan3")

# Add a legend
legend("bottomright", legend = c("Baseline", "WGI", "CPIA"), 
       col = c("cyan2", "darkcyan", "cyan3"), lwd = 2)



# Comparing variable importances
# Extract variable importance and select the top 10 variables for each model
ppd_only_top10 <- varImp(ppd_only_ranger_bi)$importance %>% as.data.frame() %>% 
  mutate(Variable = rownames(.)) %>%
  arrange(desc(Overall)) %>%
  head(10)

ppd_only_skew_top10 <- varImp(ppd_only_skew_ranger_bi)$importance %>% as.data.frame() %>% 
  mutate(Variable = rownames(.)) %>%
  arrange(desc(Overall)) %>%
  head(10)

ppd_wgi_top10 <- varImp(ppd_wgi_ranger_bi)$importance %>% as.data.frame() %>% 
  mutate(Variable = rownames(.)) %>%
  arrange(desc(Overall)) %>%
  head(10)

ppd_wgi_skew_top10 <- varImp(ppd_wgi_skew_ranger_bi)$importance %>% as.data.frame() %>% 
  mutate(Variable = rownames(.)) %>%
  arrange(desc(Overall)) %>%
  head(10)

ppd_cpia_top10 <- varImp(ppd_cpia_ranger_bi)$importance %>% as.data.frame() %>% 
  mutate(Variable = rownames(.)) %>%
  arrange(desc(Overall)) %>%
  head(10)

ppd_cpia_skew_top10 <- varImp(ppd_cpia_skew_ranger_bi)$importance %>% as.data.frame() %>% 
  mutate(Variable = rownames(.)) %>%
  arrange(desc(Overall)) %>%
  head(10)

# Combine the top 10 variables from all models into one data frame for comparison
variable_importances <- bind_rows(
  ppd_only_top10 %>% mutate(Model = "PPD_Only"),
  ppd_only_skew_top10 %>% mutate(Model = "PPD_Only_Skew"),
  ppd_wgi_top10 %>% mutate(Model = "PPD_WGI"),
  ppd_wgi_skew_top10 %>% mutate(Model = "PPD_WGI_Skew"),
  ppd_cpia_top10 %>% mutate(Model = "PPD_CPIA"),
  ppd_cpia_skew_top10 %>% mutate(Model = "PPD_CPIA_Skew")
)

# Transform the data to wide format for side-by-side comparison
wide_variable_importances <- variable_importances %>%
  pivot_wider(names_from = Model, values_from = Overall, values_fill = list(Overall = 0))

# View the resulting wide format data
print(wide_variable_importances)

# Melt the data back to long format 
long_variable_importances <- wide_variable_importances %>%
  pivot_longer(cols = -Variable, names_to = "Model", values_to = "Importance")

variable_summary <- long_variable_importances %>%
  group_by(Variable) %>%
  summarize(
    AvgImportance = mean(Importance, na.rm = TRUE),  
    Count = n(),  
    MedianImportance = mean(Importance, na.rm = TRUE)  
  ) %>%
  ungroup()

model_count <- 6 
significant_threshold <- model_count / 2  # Variables appearing in more than half the models

significant_variables <- variable_summary %>%
  filter(Count > significant_threshold & AvgImportance > 10)  

print(significant_variables)

ggplot(significant_variables, aes(x = reorder(Variable, AvgImportance), y = AvgImportance, fill = AvgImportance)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +  # Flips the axes to make variable names readable
  labs(x = "Variable", y = "Average Importance", title = "High Importance Variables Across Models")
