# -*- coding: utf-8 -*-
"""Survey Data Analysis Code

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1upnZ0M32xO8hduQddhWspvEvgBCnAwxb
"""

import pandas as pd
from collections import Counter

# Load the dataset
data = pd.read_csv("anonymous_survey_results.csv")

# Clean up the data by removing the first two metadata rows
data_cleaned = data.iloc[2:]

# Convert relevant columns to numeric where needed
prediction_columns = [col for col in data_cleaned.columns if col.endswith(tuple([str(n) for n in range(10)]))]
data_cleaned[prediction_columns] = data_cleaned[prediction_columns].apply(pd.to_numeric, errors='coerce')

# Calculate overall median prediction
overall_median_prediction = data_cleaned[prediction_columns].median().median()

# Identify importance columns and calculate importance counts
importance_columns = [col for col in data_cleaned.columns if 'EQ' in col and not col.endswith(tuple([str(n) for n in range(10)]))]
importance_counts = Counter()
for col in importance_columns:
    data_cleaned[col].apply(lambda cell: importance_counts.update([item.strip() for item in str(cell).split(',') if item != 'nan']))

# Separate data by question types and calculate median predictions for each
a_columns = [col for col in prediction_columns if 'A' in col]
w_columns = [col for col in prediction_columns if 'W' in col]
c_columns = [col for col in prediction_columns if 'C' in col]
baseline_median = data_cleaned[a_columns].median().median()
wgi_median = data_cleaned[w_columns].median().median()
cpia_median = data_cleaned[c_columns].median().median()

# Convert predictions to binary (0-3 -> 0, 4-6 -> 1) and calculate distributions
binary_predictions = data_cleaned[prediction_columns].applymap(lambda x: 1 if x >= 4 else 0 if pd.notna(x) else pd.NA)
overall_binary_distribution = binary_predictions.stack().value_counts()
baseline_binary_distribution = binary_predictions[a_columns].stack().value_counts()
wgi_binary_distribution = binary_predictions[w_columns].stack().value_counts()
cpia_binary_distribution = binary_predictions[c_columns].stack().value_counts()

# Load and analyze sparse CSV with columns ending with "-3"
sparse_file_path = '/path/to/your/sparse_dataset.csv'
sparse_data = pd.read_csv(sparse_file_path)
phrase_counts = Counter()
for column in sparse_data.columns:
    sparse_data[column].apply(lambda x: phrase_counts.update([phrase.strip() for phrase in str(x).split(',') if phrase != 'nan']))

# Display all results
print("Overall Median Prediction:", overall_median_prediction)
print("Importance Counts:", importance_counts.most_common())
print("Median Predictions by Type - Baseline:", baseline_median, "WGI:", wgi_median, "CPIA:", cpia_median)
print("Overall Binary Distribution:", overall_binary_distribution)
print("Binary Distribution by Type - Baseline:", baseline_binary_distribution, "WGI:", wgi_binary_distribution, "CPIA:", cpia_binary_distribution)
print("Phrase Counts from '-3' Columns:", phrase_counts.most_common())

# Load the actual scores and predictions
actual_scores = pd.read_csv('/path/to/actual_binary_scores.csv')
predictions = pd.read_csv('/path/to/pivoted_numeric_predictions.csv')

# Preprocess predictions
predictions['index'] = predictions['Unnamed: 0'].str.extract(r'EQ-(\d+)').astype(int)
predictions['question_type'] = predictions['Unnamed: 0'].str.extract(r'EQ-\d+([A-Z])-')[0]
predictions['prediction'] = predictions.iloc[:, 1:-2].apply(lambda row: row.dropna().iloc[0] if not row.dropna().empty else 0, axis=1)
predictions['bi_pred_regular'] = predictions['prediction'].apply(lambda x: 0 if 0 <= x <= 4 else 1)
predictions = predictions[['index', 'question_type', 'bi_pred_regular']]

# Merge datasets
merged_data = pd.merge(actual_scores, predictions, on='index', how='left')

# Calculate overall accuracy
overall_accuracy_regular = (merged_data['bi_overall_rating'] == merged_data['bi_pred_regular']).mean() * 100

# Apply skewed cutoff
merged_data['bi_pred_skewed'] = merged_data['prediction'].apply(lambda x: 0 if 0 <= x <= 4 else 1)
overall_accuracy_skewed = (merged_data['bi_overall_rating'] == merged_data['bi_pred_skewed']).mean() * 100

# Prepare data for plotting
accuracy_data = {
    'Accuracy Type': ['Overall', 'Type A', 'Type C', 'Type W', 'Overall', 'Type A', 'Type C', 'Type W'],
    'Accuracy': [overall_accuracy_regular, 46.81, 46.81, 47.31, overall_accuracy_skewed, 42.55, 48.94, 50.54],
    'Model': ['Regular', 'Regular', 'Regular', 'Regular', 'Skewed', 'Skewed', 'Skewed', 'Skewed']
}
df_accuracies = pd.DataFrame(accuracy_data)

# Plotting
plt.figure(figsize=(14, 6))
for i, model_type in enumerate(['Regular', 'Skewed']):
    plt.subplot(1, 2, i+1)
    sns.barplot(x='Accuracy Type', y='Accuracy', data=df_accuracies[df_accuracies['Model'] == model_type], palette=['darkcyan' if model_type == 'Regular' else 'lightblue'])
    plt.title(f'{model_type} Model Accuracies')
    plt.ylim(0, 100)
    plt.xticks(rotation=45)
    plt.ylabel('Accuracy (%)')
    plt.xlabel('Accuracy Type')
plt.tight_layout()
plt.show()