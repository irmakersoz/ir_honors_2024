### Stanford University ###
### IR Honors Thesis ###
##### 2023-2024 #####
#### Irmak Ersoz #### 

# Methods Plots: Replication Code

# Load necessary libraries
library(ggplot2)

# Plot six_overall_score distribution for each predictor matrix
ggplot(ppd_only, aes(x = six_overall_rating)) +
  geom_histogram(binwidth = 1, fill = "darkcyan", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of six_overall_score, Project Characteristics Only", 
       x = "Performance Score (1-6)", y = "Frequency") +
  scale_x_continuous(breaks = 1:6)

ggplot(ppd_wgi, aes(x = six_overall_rating)) +
  geom_histogram(binwidth = 1, fill = "darkcyan", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of six_overall_score, with WGI", 
       x = "Performance Score (1-6)", y = "Frequency") +
  scale_x_continuous(breaks = 1:6)


ggplot(ppd_cpia, aes(x = six_overall_rating)) +
  geom_histogram(binwidth = 1, fill = "darkcyan", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of six_overall_score, with CPIA", 
       x = "Performance Score (1-6)", y = "Frequency") +
  scale_x_continuous(breaks = 1:6)




# Plot bi_overall_rating for each predictor matrix
ggplot(ppd_only, aes(x = bi_overall_rating)) +
  geom_bar(fill = "darkcyan") +
  theme_minimal() +
  labs(x = "Performance Score Categories", y = "Project Count") +
  ggtitle("Bar Plot of Binary Overall Score, Project Characteristics Only")


ggplot(ppd_wgi, aes(x = bi_overall_rating)) +
  geom_bar(fill = "darkcyan") +
  theme_minimal() +
  labs(x = "Performance Score Categories", y = "Project Count") +
  ggtitle("Bar Plot of Binary Overall Score, with WGI")


ggplot(ppd_cpia, aes(x = bi_overall_rating)) +
  geom_bar(fill = "darkcyan") +
  theme_minimal() +
  labs(x = "Performance Score Categories", y = "Project Count") +
  ggtitle("Bar Plot of Binary Overall Score, with CPIA")



# Plot bi_overall_rating distribution after SMOTE 
ggplot(baseline_smote, aes(x = bi_overall_rating)) +
  geom_bar(fill = "darkcyan") +
  theme_minimal() +
  labs(x = "Performance Score Categories", y = "Project Count") +
  ggtitle("Bar Plot of Binary Performance Score after SMOTE, Project Characteristics Only")

ggplot(wgi_smote, aes(x = bi_overall_rating)) +
  geom_bar(fill = "darkcyan") +
  theme_minimal() +
  labs(x = "Performance Score Categories", y = "Project Count") +
  ggtitle("Bar Plot of Binary Performance Score after SMOTE, with WGI")

ggplot(cpia_smote, aes(x = bi_overall_rating)) +
  geom_bar(fill = "darkcyan") +
  theme_minimal() +
  labs(x = "Performance Score Categories", y = "Project Count") +
  ggtitle("Bar Plot of Binary Performance Score after SMOTE, with CPIA")



# Plot bi_overall_rating distribution after skewed cutoff 
ggplot(ppd_only_skew, aes(x = bi_overall_rating_skew)) +
  geom_bar(fill = "darkcyan") +
  theme_minimal() +
  labs(x = "Performance Score Categories", y = "Project Count") +
  ggtitle("Bar Plot of Skewed Cutoff Binary Overall Score, Project Characteristics Only")

ggplot(ppd_wgi_skew, aes(x = bi_overall_rating_skew)) +
  geom_bar(fill = "darkcyan") +
  theme_minimal() +
  labs(x = "Performance Score Categories", y = "Project Count") +
  ggtitle("Bar Plot of Skewed Cutoff Binary Overall Score, with WGI")

ggplot(ppd_cpia_skew, aes(x = bi_overall_rating_skew)) +
  geom_bar(fill = "darkcyan") +
  theme_minimal() +
  labs(x = "Performance Score Categories", y = "Project Count") +
  ggtitle("Bar Plot of Skewed Cutoff Binary Overall Score, with CPIA")
