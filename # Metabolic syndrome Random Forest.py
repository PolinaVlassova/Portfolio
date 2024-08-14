# Metabolic syndrome Random Forest

# Importing necessary libraries
import pandas as pd
import numpy as np
from sklearn.preprocessing import OrdinalEncoder
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score
from yellowbrick.model_selection import validation_curve
from sklearn.metrics import confusion_matrix
import matplotlib.pyplot as plt

# Load the dataset
# 'r' before the string is important for Windows paths to avoid escape sequences
df = pd.read_csv(r'C:\Users\vlassova\Downloads\Metabolic Syndrome.csv')

# Display the first few rows of the dataset
df.head()

# Show descriptive statistics for the dataset
df.describe()

# Drop the 'seqn' column as it's likely an identifier not needed for modeling
df = df.drop(columns=['seqn'])

# Check for missing values in the dataset
df.isna().sum()

# Encode categorical variables into numerical ones
oe = OrdinalEncoder()

# Encode the 'MetabolicSyndrome' column (convert categorical to numeric)
df['MetabolicSyndrome'] = oe.fit_transform(df[['MetabolicSyndrome']])

# Encode the 'Sex' column (male = 1, female = 0)
df['Sex'] = oe.fit_transform(df[['Sex']])

# Display the dataframe to inspect the changes
df

# Visualize the distribution of 'Sex' in the dataset
sns.countplot(data=df, x='Sex')

# Check the distribution of the 'Sex' column
df['Sex']

# Display the count of each class in the 'MetabolicSyndrome' column
df.MetabolicSyndrome.value_counts()

# Sample 500 rows from the dataset without replacement
df_sample = df.sample(n=500, replace=False, random_state=42)

# Check the distribution of the 'MetabolicSyndrome' column in the sample
df_sample.MetabolicSyndrome.value_counts()

# Replace 1s with 0s and 0s with 1s in the 'MetabolicSyndrome' column (flipping the values)
df_sample['MetabolicSyndrome'] = df_sample['MetabolicSyndrome'].replace({1:0, 0:1})

# Display the modified sample
df_sample

# Fill missing values in the 'BMI' column with the mean value of that column
mean = np.mean(df['BMI'])
df_sample['BMI'] = df_sample['BMI'].fillna(mean)

# Calculate the first and third quartiles of the 'BMI' column
q1 = np.percentile(df_sample['BMI'], 25)
q3 = np.percentile(df_sample['BMI'], 75)

# Calculate the interquartile range (IQR)
iqr = q3 - q1

# Filter out outliers outside the IQR range
df_filt = df_sample[(df_sample['BMI'] >= q1) & (df_sample['BMI'] <= q3)]

# Check the maximum value of 'BMI' after filtering
df_filt['BMI'].max()

# Further filter the dataset to include only rows where 'Race' is "White"
df_filt = df_filt[(df_filt['Race'] == "White")]

# Encode the 'Marital' column into numerical format
df_filt['Marital'] = oe.fit_transform(df_filt[['Marital']])

# Verify the data type of the 'Marital' column to ensure it's numeric
df_filt['Marital'].dtype

# Drop the 'Race' column as it's now unnecessary
df_filt = df_filt.drop(columns='Race')

# Define the feature set (all columns except the last one)
x = df_filt.iloc[:, 0:-1]

# Define the target variable (the last column)
y = df_filt.iloc[:, -1]

# Split the data into training and testing sets (80% train, 20% test)
x_train, x_test, y_train, y_test = train_test_split(x, y, train_size=0.8, test_size=0.2)

# Display the training set to inspect the data
x_train

# Initialize the RandomForestClassifier
clf1 = RandomForestClassifier()

# Train the model on the training data
clf1.fit(x_train, y_train)

# Make predictions on the test data
pred = clf1.predict(x_test)

# Calculate the accuracy of the model
acc = accuracy_score(pred, y_test)

# Print the accuracy score
print(f'Accuracy score: {acc}')

# Hyperparameters tuning

# Define the range of values for 'min_samples_split' parameter
num_est = [5, 10, 20, 30, 40, 50]

# Perform a validation curve analysis on 'min_samples_split' with 4-fold cross-validation
print(validation_curve(RandomForestClassifier(), X=x_train, y=y_train, param_name='min_samples_split', param_range=num_est, scoring='accuracy', cv=4))  # 4-fold CV
# Define the maximum depth range for the RandomForest model
max_depth = [1, 5, 10, 15, 20, 25]

# Perform validation curve analysis to evaluate the effect of 'max_depth' on accuracy
# using 4-fold cross-validation
print(validation_curve(RandomForestClassifier(), X=x_train, y=y_train, param_name='max_depth', param_range=max_depth, scoring='accuracy', cv=4))  # Evaluate with CV=4

# Initialize a RandomForestClassifier with tuned parameters: 5 trees and maximum depth of 5
clf2 = RandomForestClassifier(n_estimators=5, max_depth=5, random_state=42)

# Train the classifier on the training data
clf2.fit(x_train, y_train)

# Make predictions on the test data
pred2 = clf2.predict(x_test)

# Calculate and print the accuracy of the tuned model
acc2 = accuracy_score(pred2, y_test)
print(f'Tuned model accuracy score: {acc2}')

# Check the importance of each feature
# Generate a series with feature importances, sorted in descending order
feature_scores = pd.Series(clf2.feature_importances_, index=x_train.columns).sort_values(ascending=False)

# Print the feature importances
print(feature_scores)

# Visualize the feature importances using a barplot
sns.barplot(x=feature_scores, y=feature_scores.index)

# Further refine the dataset by dropping less important columns based on feature importance
df = df_filt.drop(columns=["Sex", 'Income', 'Marital', 'UrAlbCr', 'HDL', 'Age'])

# Redefine the feature set (x) and the target variable (y) after dropping the columns
x = df.iloc[:, 0:-1]
y = df.iloc[:, -1]

# Split the refined dataset into training and testing sets (80% train, 20% test)
x_train, x_test, y_train, y_test = train_test_split(x, y, train_size=0.8, test_size=0.2, random_state=42)

# Display the shapes of the training data to ensure correct splitting
x_train.shape, y_train.shape

# Train the classifier again on the refined dataset
clf2.fit(x_train, y_train)

# Make predictions on the test data using the refined model
pred = clf2.predict(x_test)

# Calculate and print the accuracy of the final model
acc3 = accuracy_score(pred, y_test)
print(f'Accuracy score of the final model: {acc3}')

# Generate and display the confusion matrix to evaluate the performance of the final model
print(confusion_matrix(y_test, pred))

