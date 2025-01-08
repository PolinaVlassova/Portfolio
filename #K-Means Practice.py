#   K-Means Practice: Wine Dataset Clustering

# Import necessary libraries
import pandas as pd
import numpy as np
from sklearn import datasets
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans
from sklearn.metrics import accuracy_score
import matplotlib.pyplot as plt

# Load the Wine dataset
data = datasets.load_wine()

# Define feature matrix (X) and target vector (y)
X = data.data
y = data.target

# Create a DataFrame for better visualization and manipulation
df = pd.DataFrame(X, columns=data.feature_names)
df['Wine Class'] = y
print(df.head())

# Check for null values
print(df.isnull().sum())

# Display descriptive statistics
print(df.describe())  # Mean, std. dev., etc.

# Standardize the data
sc = StandardScaler()
X_scaled = sc.fit_transform(X)

# Determine the optimal number of clusters using the elbow method
wss = []
for i in range(1, 11):
    kmeans = KMeans(n_clusters=i, init='k-means++', random_state=0)
    kmeans.fit(X_scaled)
    wss.append(kmeans.inertia_)

# Plot the elbow graph
plt.figure(figsize=(8, 6))
plt.plot(range(1, 11), wss, marker='o')
plt.title('The Elbow Method')
plt.xlabel('Number of Clusters')
plt.ylabel('Within-cluster Sum of Squares (WSS)')
plt.show()

# Set the number of clusters
n_clusters = 3

# Initialize and fit the KMeans model
k_means = KMeans(init='k-means++', n_clusters=n_clusters, n_init=10, max_iter=360, random_state=0)
k_means.fit(X_scaled)

# Get the cluster labels
labels = k_means.labels_
print(labels)

# Calculate and print the accuracy score
print("Accuracy Score:", accuracy_score(labels, y))
