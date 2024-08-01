# Decision Trees Practice

# Import necessary libraries
import pandas as pd
import numpy as np
import zipfile
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.preprocessing import OrdinalEncoder
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import accuracy_score
from sklearn import tree

# Path to the ZIP file
zip_file_path = r'C:\Users\vlassova\Downloads\archive.zip'

# Extract the CSV file from the ZIP archive
with zipfile.ZipFile(zip_file_path, 'r') as zip_ref:
    zip_ref.extractall('C:\\Users\\vlassova\\Downloads\\extracted_files')

# Path to the extracted CSV file
csv_file_path = r'C:\Users\vlassova\Downloads\extracted_files\drug200.csv'

# Load the dataset
df = pd.read_csv(csv_file_path)
print(df.head())

# Check for null values
print(df.isnull().sum())

# Check for duplicated entries
print(df.duplicated().sum())
print(df[df.duplicated()])

# Display dataframe information
df.info()

# Display value counts for 'Sex'
x = df.Sex.value_counts()
print(x)

# Visualize the distribution of 'Sex'
sns.countplot(data=df, x='Sex')
plt.show()

# Display value counts for 'Drug'
plt.figure(figsize=(5, 5))
z = df.Drug.value_counts()
y = sorted(df['Drug'].unique())  # Sort alphabetically
print(z)

# Visualize the distribution of 'Drug'
sns.countplot(data=df, x='Drug', palette=['Green', 'Blue'], order=y)
plt.show()

# Display value counts for 'Cholesterol'
plt.figure(figsize=(5, 5))
y = df.Cholesterol.value_counts().sort_values(ascending=True)
print(y)

# Visualize the distribution of 'Cholesterol'
sns.countplot(data=df, x='Cholesterol', palette=['Orange', 'Pink'])
plt.show()

# Visualize the distribution of 'Age' for each 'Drug' class
plt.figure(figsize=(8, 8))
sns.distplot(df[df['Drug'] == 'drugA']['Age'], color='green', label='drugA')
sns.distplot(df[df['Drug'] == 'drugB']['Age'], color='red', label='drugB')
sns.distplot(df[df['Drug'] == 'drugC']['Age'], color='pink', label='drugC')
sns.distplot(df[df['Drug'] == 'drugX']['Age'], color='orange', label='drugX')
sns.distplot(df[df['Drug'] == 'drugY']['Age'], color='blue', label='drugY')
plt.title('Age vs. Drug Class')
plt.legend()
plt.show()

# Encode categorical features
oe = OrdinalEncoder()
df['BP'] = oe.fit_transform(df[['BP']])
df['Cholesterol'] = oe.fit_transform(df[['Cholesterol']])
df['Sex'] = oe.fit_transform(df[['Sex']])
df['Drug'] = oe.fit_transform(df[['Drug']])
print(df.head())

# Define feature matrix (X) and target vector (y)
X = df.iloc[:, 0:-1]
y = df.iloc[:, -1]

# Split the dataset into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=1)

# Display information about the training and testing sets
X_train.info()
X_test.info()

# Initialize the Decision Tree classifier with Gini criterion
clf_gini = DecisionTreeClassifier(criterion='gini', random_state=0)

# Train the classifier
clf_gini.fit(X_train, y_train)

# Predict the target values for the test set
y_pred_gini = clf_gini.predict(X_test)

# Calculate and print the accuracy score
print("Gini Accuracy Score:", accuracy_score(y_test, y_pred_gini))

# Visualize the Decision Tree
plt.figure(figsize=(10, 10))
tree.plot_tree(clf_gini.fit(X_train, y_train))
plt.show()

# Initialize the Decision Tree classifier with Entropy criterion and a max depth of 3
clf_entropy = DecisionTreeClassifier(criterion='entropy', max_depth=3)
clf_entropy.fit(X_train, y_train)

# Predict the target values for the test set
y_pred_entropy = clf_entropy.predict(X_test)

# Calculate and print the accuracy score
print("Entropy Accuracy Score:", accuracy_score(y_test, y_pred_entropy))
