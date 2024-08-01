# Random Forest Practice

# Import necessary libraries
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, f1_score, precision_score, confusion_matrix, classification_report
from sklearn.preprocessing import OrdinalEncoder
from sklearn.model_selection import train_test_split
from yellowbrick.model_selection import validation_curve
import seaborn as sns
import matplotlib.pyplot as plt

# Load the dataset
df = pd.read_csv('C:\\Users\\vlassova\\Downloads\\car_evaluation.csv')

# Define column names
col_names = ['buying', 'maint', 'doors', 'persons', 'lug_boot', 'safety', 'class']
df.columns = col_names

# Display the dataframe
print(df.head())

# Display dataframe information
df.info()  # No null entries

# Check for duplicated entries
print(df[df.duplicated()])  # No duplicates

# Display descriptive statistics
print(df.describe(include='all').T)  # Transpose for better visualization

# Display value counts for each column
for col in col_names:
    print(df[col].value_counts())

# Encode categorical features
oe = OrdinalEncoder()
for col in df:
    df[col] = oe.fit_transform(df[[col]])

# Define feature matrix (X) and target vector (y)
X = df.iloc[:, 0:-1]
y = df.iloc[:, -1]

# Split the dataset into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

# Display information about the training and testing sets
X_train.info()
X_test.info()

# Initialize the Random Forest classifier
clf1 = RandomForestClassifier()

# Train the classifier
clf1.fit(X_train, y_train)

# Predict the target values for the test set
pred1 = clf1.predict(X_test)

# Calculate and print the accuracy score
print("Accuracy Score:", accuracy_score(y_test, pred1))

# Hyperparameter tuning using validation curves
# Define the range of number of estimators
num_est = [100, 200, 450, 500, 750, 1000]
print(validation_curve(RandomForestClassifier(), X=X_train, y=y_train, param_name='n_estimators', param_range=num_est, scoring='accuracy', cv=3))

# Define the range of maximum depths
depth_vals = [10, 7, 15, 12, 20, 22]
print(validation_curve(RandomForestClassifier(), X=X_train, y=y_train, param_name='max_depth', param_range=depth_vals, scoring='accuracy', cv=3))

# Define the range of minimum samples split
min_samples = [3, 6, 4, 5, 8, 12, 15]
print(validation_curve(RandomForestClassifier(), X=X_train, y=y_train, param_name='min_samples_split', param_range=min_samples, scoring='accuracy', cv=3))

# Initialize a new Random Forest classifier with tuned hyperparameters
clf2 = RandomForestClassifier(n_estimators=500, min_samples_split=3, max_depth=15, random_state=0)
clf2.fit(X_train, y_train)

# Predict the target values for the test set
pred2 = clf2.predict(X_test)

# Calculate and print the accuracy score
print("Tuned Model Accuracy Score:", accuracy_score(y_test, pred2))

# Check the importance of each feature
feature_scores = pd.Series(clf2.feature_importances_, index=X_train.columns).sort_values(ascending=False)
print(feature_scores)

# Visualize the feature importances
sns.barplot(x=feature_scores, y=feature_scores.index, palette=['pink', 'orange', 'violet'])
plt.title('Visualization of Feature Importances')
plt.show()

# Drop less important features
df.drop(columns=['maint', 'doors', 'lug_boot'], inplace=True)

# Define the feature matrix (X) and target vector (y) again
X = df.iloc[:, 0:-1]
y = df.iloc[:, -1]

# Split the dataset into training and testing sets again
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=0)

# Initialize another Random Forest classifier
clf3 = RandomForestClassifier()
clf3.fit(X_train, y_train)

# Predict the target values for the test set
pred3 = clf3.predict(X_test)

# Calculate and print the accuracy score
print("Final Model Accuracy Score:", accuracy_score(y_test, pred3))
