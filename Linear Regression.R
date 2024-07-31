
directory <- setwd("/Users/polinavlasova/Downloads")
data <- read.csv("Medicalpremium.csv")
head(data)

str(data)

summary(data)



# DATA CLEANING
# Check for missing values
missing_values <- sum(is.na(data))
missing_values
# There are no missing values in the dataset.

# Check for duplicates
sum(duplicated(data))
# There are no duplicate records in the dataset.

# Check for data consistency across binary variables

unique(data$Diabetes)
unique(data$BloodPressureProblems)
unique(data$AnyTransplants)
unique(data$AnyChronicDiseases)
unique(data$KnownAllergies)
unique(data$HistoryOfCancerInFamily)

# 0 1, verifies binary consistency for all binary variables

# Summary of cleaning data
# These results confirm that our dataset is clean, complete, and consistent.



# Exploratory Data Analysis

library(ggplot2)
library(dplyr)
library(stats)

summary(data)

# Histograms for continuous variables like Age, Height, Weight, and PremiumPrice
ggplot(data, aes(x=Age)) + geom_histogram(bins=30, fill="blue", color="black") + ggtitle("Distribution of Age")
ggplot(data, aes(x=Height)) + geom_histogram(bins=30, fill="green", color="black") + ggtitle("Distribution of Height")
ggplot(data, aes(x=Weight)) + geom_histogram(bins=30, fill="red", color="black") + ggtitle("Distribution of Weight")
ggplot(data, aes(x=PremiumPrice)) + geom_histogram(bins=30, fill="purple", color="black") + ggtitle("Distribution of Premium Price")

# Histogram showing the distribution of:
# Age: The distribution is relatively uniform with small peaks around 45 years, meaning a diverse age range in the dataset.
# Height: The distribution has a peak at around 164 cm, meaning a common height range among the subjects.
# Weight: The distribution shows a peak slightly to the left of 75 kg, meaning that the most common weight range among the subjects is just below 75 kg.
# Premium Price: The distribution is multimodal, with significant peaks around 14000 and 23000, meaning popular pricing tiers in the dataset.


# Boxplots for continuous variables by categorical variables (e.g., Diabetes)
ggplot(data, aes(x=factor(Diabetes), y=PremiumPrice)) + geom_boxplot() + ggtitle("Premium Price by Diabetes Status")
ggplot(data, aes(x=factor(BloodPressureProblems), y=PremiumPrice)) + geom_boxplot() + ggtitle("Premium Price by Blood Pressure Problems")


# Correlation matrix for continuous variables
corr_matrix <- cor(data[c("Age", "Height", "Weight", "PremiumPrice")])

# Age and PremiumPrice (0.70): This strong positive correlation indicates that as individuals age, their insurance premiums tend to increase.
# Height and PremiumPrice (0.03): There's a negligible correlation between height and premium price, suggesting height does not play a significant role in insurance cost determination.
# Weight and PremiumPrice (0.14): A modest positive correlation exists, hinting that heavier individuals may face slightly higher premiums, though the effect is not strong.
# Height and Weight (0.07): A weak positive correlation, reflecting expected physiological relationships where taller individuals might weigh more, though it's of limited consequence for premium pricing.



# REGRESSION MODEL

library(caret)
library(car)
library(ggplot2)

# Split the dataset 
set.seed(123) 
index <- createDataPartition(data$PremiumPrice, p = 0.8, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]


full_model <- lm(PremiumPrice ~ ., data = train_data)

model_summary <- summary(full_model)
model_summary

# The model's R-squared of 0.63 suggests that approximately 63% of the variance in PremiumPrice can be explained by the model.
# Significant predictors include Age, AnyTransplants, AnyChronicDiseases, Weight, HistoryOfCancerInFamily, and NumberOfMajorSurgeries, indicating strong relationships with PremiumPrice.

# Multicollinearity check
vif_results <- vif(full_model)
vif_results 
# All VIF values are below 5, meaning no significant multicollinearity among predictors.

# ANOVA 
anova_results <- anova(full_model)
anova_results  

# Age, Diabetes, AnyTransplants, AnyChronicDiseases, Weight, HistoryOfCancerInFamily, NumberOfMajorSurgeries are statistically significant with the p-values less than 5, meaning these variables have some impact on the Premium Price.
# BloodPressureProblems, Height, KnownAllergies: Not significant with high p-values, suggesting they don’t influence PremiumPrice significantly.


# Predict on the test set using the full model
predictions <- predict(full_model, test_data)

# Evaluate model performance
test_data$predictions <- predictions
results <- postResample(pred = predictions, obs = test_data$PremiumPrice)
results
# RMSE: 3246.33, meaning the average deviation of the predictions from actual values.
# R-squared: 0.711, meaning that 71.1% of the variance in PremiumPrice can be explained by the model.
# MAE: 2520.85, meaning the average absolute error in the predictions.
# This suggests that the model performs well in predicting insurance premiums, capturing a significant portion of the variance.


# Visualize the results
ggplot(test_data, aes(x = PremiumPrice, y = predictions)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Actual vs Predicted Premium Prices", x = "Actual Prices", y = "Predicted Prices")
# The plot shows how predicted values stack up against actual values, helping visualize the accuracy of the model.

