# add correlation for all variables


directory <- setwd("/Users/polinavlasova/Downloads")
data <- read.csv("Metabolic Syndrome.csv")
str(data)
install.packages("caret")
install.packages("pROC")
library(titanic)
library(tidyverse)
library(caret)
library(pROC)
library(car)
# Data Exploriation
nrow(data)
data <- na.omit(data)
data <- data[data$Marital != "", ]
ones <- data[data$MetabolicSyndrome == 1, ]
zeroes <- data[data$MetabolicSyndrome == 0, ]
nrow(ones)/nrow(data)
nrow(zeroes)/nrow(data)
nrow(ones)
nrow(zeroes)

# Balancing the dataframe
# Separate the data into two groups: ones and zeroes
ones <- data[data$MetabolicSyndrome == 1, ]
zeroes <- data[data$MetabolicSyndrome == 0, ]

# Sample the same number of observations from each group with replacement
sampled_ones <- ones[sample(nrow(ones), size = min(nrow(ones)), replace = TRUE), ]
sampled_zeroes <- zeroes[sample(nrow(zeroes), size = min(nrow(ones))), ]

# Combine the sampled data
balanced_data <- rbind(sampled_ones, sampled_zeroes)

# Reshuffle the combined data
balanced_data <- balanced_data[sample(nrow(balanced_data)), ]

# Check of final proportion
ones <- balanced_data[balanced_data$MetabolicSyndrome == 1, ]
zeroes <- balanced_data[balanced_data$MetabolicSyndrome == 0, ]
nrow(ones)/nrow(balanced_data)
nrow(zeroes)/nrow(balanced_data)
nrow(ones) 
nrow(zeroes) 
str(balanced_data)

training_index = createDataPartition(balanced_data$MetabolicSyndrome, p = 0.8, list = FALSE)
training_data = balanced_data[training_index,]
test_data = balanced_data[-training_index,]

# Regression for sex
model1 <- glm(MetabolicSyndrome~as.factor(Sex), training_data, family=binomial)
summary(model1)
exp(coef(model1))
# Each coefficient that has been exponentiated (like for SexMale) tells you how the odds 
# of the outcome change with a one-unit change in the predictor, relative to the reference category.
# In your case, males have a 18.4% higher odds of the outcome compared to females, given that the odds ratio is more than 1.

# Mutlicollinearity
model2 <- glm(MetabolicSyndrome~BMI+WaistCirc+HDL, training_data, family=binomial)
vif(model2)
# The VIF values for BMI and WaistCirc are higher, suggesting that there may be some degree
# of multicollinearity between these variables and possibly with each other. These values are
# above the typical threshold of 5, which may warrant further investigation into multicollinearity 
# issues.
# The VIF for HDL is relatively low, indicating low multicollinearity with the other predictor variables. 
# Generally, a VIF below 2 is considered low.

data_females <- na.omit(balanced_data[balanced_data$Sex=="Female",])
data_males <- na.omit(balanced_data[balanced_data$Sex=="Male",])
nrow(training_data[training_data$Sex=="Male",])
nrow(training_data[training_data$Sex=="Female",])

#Multicollinearity testing

model1 <- glm(MetabolicSyndrome~BMI+WaistCirc+HDL, training_data, family=binomial)
summary(model1)
vif(model)

model2 <- glm(MetabolicSyndrome ~ log(UrAlbCr)+Albuminuria, training_data, family=binomial)
summary(model2)
vif(model2) 
cor(balanced_data$UrAlbCr, balanced_data$Albuminuria)
plot(model2$residuals)

#ANOVA test for differences in Marital Status   
anova_result <- aov(MetabolicSyndrome~Marital, training_data) 
summary(anova_result) 
TukeyHSD(anova_result)
plot(TukeyHSD(anova_result))
# ANCOVA doesn't fit because there is no continuous variable
ancova_result <- aov(BMI~Marital, training_data) 
summary(ancova_result)
TukeyHSD(ancova_result)
plot(TukeyHSD(ancova_result))
# significant differences between widowed and single individuals
model3 <- glm(MetabolicSyndrome~BMI, data_females, family=binomial)
summary(model3)
plot(model3$residuals)
shapiro_test <- shapiro.test(residuals(model3))
shapiro_test 
library(lmtest)
bp_test <- bptest(model3)
bp_test 
qqnorm(model3$residuals)

model4 <- glm(MetabolicSyndrome~BMI, data_males, family=binomial)
summary(model4)
plot(model4$residuals)
shapiro_test <- shapiro.test(residuals(model4))
shapiro_test
bp_test <- bptest(model4)
bp_test
# looop over variables and determine normalitu or homoscedasticity
library(MASS)
hist(training_data$Triglycerides)
balanced_data <- training_data[training_data$Triglycerides<1000,]
balanced_data <- training_data[training_data$BloodGlucose<300,]
model5 <- glm(MetabolicSyndrome~BloodGlucose+Triglycerides, training_data, 
              family=binomial)
summary(model5)
exp(coef(model5))
prediction = predict(model5, test_data, type= "response")
prediction

predicted = ifelse(prediction> 0.5, "1", "0")

conf_matrix = table(predicted = predicted, actual = test_data$MetabolicSyndrome)

conf_matrix

# Accuracy
sum(diag(conf_matrix))/sum(conf_matrix)
roc_curve = roc(as.numeric(test_data$MetabolicSyndrome), as.numeric(predicted))
plot(roc_curve)
plot(model5$residuals)
shapiro.test(model5$residuals)
bptest(model5)


# categorical regression
nrow(data_females[data_females$Marital=="Married",])
nrow(data_females[data_females$Marital=="Divorced",])
nrow(data_females[data_females$Marital=="Separated",])
nrow(data_females[data_females$Marital=="Single",])
nrow(data_males[data_females$Marital=="Widowed",])
nrow(data_females[data_females$Race=="White",])
nrow(data_females[data_females$Race=="Hispanic",])
nrow(data_females[data_females$Race=="Black",])
nrow(data_females[data_females$Race=="Asian",])

nrow(data_males[data_males$Marital=="Married",])
nrow(data_males[data_males$Marital=="Divorced",])
nrow(data_males[data_males$Marital=="Separated",])
nrow(data_males[data_males$Marital=="Single",])
nrow(data_males[data_males$Marital=="Widowed",])
nrow(data_males[data_males$Race=="White",])
nrow(data_males[data_males$Race=="Hispanic",])
nrow(data_males[data_males$Race=="Black",])
nrow(data_females[data_males$Race=="Asian",])

# make frequency table and chi-squared test

# Logistic regression for categorical variables
model6 <- glm(MetabolicSyndrome ~ Marital + Race, training_data, family = binomial)
summary(model6)
exp(coef(model6))
prediction = predict(model6, test_data, type= "response")
prediction

predicted = ifelse(prediction> 0.5, "1", "0")

conf_matrix = table(predicted = predicted, actual = test_data$MetabolicSyndrome)

conf_matrix

# Accuracy
sum(diag(conf_matrix))/sum(conf_matrix)

# ROC and AUC
roc_curve = roc(as.numeric(test_data$MetabolicSyndrome), as.numeric(predicted))
plot(roc_curve)

auc(roc_curve)



# Final regression Model
model8 <- glm(MetabolicSyndrome ~  Income+WaistCirc+Albuminuria+
              HDL+Triglycerides,training_data, family=binomial)
summary(model8)
plot(model8$residuals)
residuals <- residuals(model8)
# Plot to assess influence
plot(model8, which=5)  # Cook's distance plot
# Identify outlier indices
outliers <- which(residuals > 2 | residuals < -2)
outliers
# Remove outliers
cleaned_data <- training_data[-outliers,]
model8 <- glm(MetabolicSyndrome ~  Income+WaistCirc+Albuminuria+
                HDL+Triglycerides,cleaned_data, family=binomial)
summary(model8)
plot(model8$residuals)
exp(coef(model8))
prediction = predict(model8, test_data, type= "response")
prediction

predicted = ifelse(prediction> 0.5, "1", "0")

conf_matrix = table(predicted = predicted, actual = test_data$MetabolicSyndrome)

conf_matrix

# Accuracy
sum(diag(conf_matrix))/sum(conf_matrix)

# ROC and AUC
roc_curve = roc(as.numeric(test_data$MetabolicSyndrome), as.numeric(predicted))
plot(roc_curve)

auc(roc_curve)
cs = seq(0.2, 0.8, by = 0.1)

for(i in cs){
  prediction = predict(model, test_data, type= "response")
  prediction
  
  predicted = ifelse(prediction> i, "1", "0")
  
  conf_matrix = table(predicted = predicted, actual = test_data$MetabolicSyndrome)
  
  sens_spec = diag(conf_matrix)/colSums(conf_matrix)
  
  cat(paste("\n Cut off:", i, "\n sensitivity: ",
            sens_spec[1], "\n specificity: ", sens_spec[2]))
}

# explain sensitivity and specificity

model9 <- glm(MetabolicSyndrome ~ Age+BMI+UrAlbCr+UricAcid,training_data,
              family=binomial)
summary(model9)
plot(model9$residuals)
outliers <- which(residuals > 2 | residuals < -2)
outliers
# Remove outliers
cleaned_data <- training_data[-outliers,]
model9 <- glm(MetabolicSyndrome ~ Age+BMI+UrAlbCr+UricAcid,cleaned_data,
              family=binomial)
summary(model9)
plot(model9$residuals)
