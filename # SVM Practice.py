# SVM Practice

# Import necessary libraries
import numpy as np
import pandas as pd
from sklearn import datasets
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.svm import SVC
from sklearn.metrics import accuracy_score, confusion_matrix

# Load the Iris dataset
data = datasets.load_iris()

# Define feature matrix (X) and target vector (y)
X = data.data
y = data.target
names = data.target_names

# Display the shapes of X and y
print(X.shape)
print(y.shape)

# Create a DataFrame for better visualization and manipulation
df = pd.DataFrame(X, columns=data.feature_names)
df['species'] = data.target
df['species'] = df['species'].replace(to_replace=[0, 1, 2], value=['setosa', 'versicolor', 'virginica'])
print(df.head())

# Split the dataset into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=1)
print(X_train.shape, y_train.shape)
print(X_test.shape, y_test.shape)

# Initialize the SVM classifier with a linear kernel
svm = SVC(kernel='linear', random_state=0)

# Train the classifier
svm.fit(X_train, y_train)

# Predict the target values for the test set
pred = svm.predict(X_test)
print(pred)

# Calculate and print the accuracy score
print("Linear SVM Accuracy Score:", accuracy_score(y_test, pred))

# Calculate and print the confusion matrix
print("Confusion Matrix:\n", confusion_matrix(y_test, pred))

# Initialize the SVM classifier with an RBF kernel
rbf_svm = SVC(kernel='rbf', random_state=0)

# Train the classifier
rbf_svm.fit(X_train, y_train)

# Predict the target values for the test set
rbf_pred = rbf_svm.predict(X_test)

# Calculate and print the accuracy score for the RBF kernel
print("RBF SVM Accuracy Score:", accuracy_score(y_test, rbf_pred))
