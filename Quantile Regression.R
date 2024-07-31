#
## Quantile Regression
#
# This example is from MICROECONOMETRICS USING STATA 
# (A. C. Cameron and P. K. Trivedi)
# R package required for quantile regression is {guantreg}

# packages
# use:  install.packages("package.name")  if necessary
install.packages("foreign")
install.packages("quantreg")
install.packages("tidyverse")
install.packages("lmtest")
library(lmtest) 
library(foreign)
library(quantreg)
library(ggplot2)
library(reshape2)
# Data loading and basic processing
dta <- read.dta("/Users/polinavlasova/Downloads/qreg.dta")
apply(dta, 2,function(x) sum(is.na(x))) # number of NA in each column - missing data
dta <- dta[,c("ltotexp", "suppins","totchr", "age", "female")] # variables for our model
dta <- na.omit(dta) # we only use "full" rows - no missing data
str(dta)
#
# Data description:
#
#  | Variable | Description                              | Type    |
#  |----------|------------------------------------------|---------|
#  | ltotexp  | log of medical expenditure               | num     |
#  | suppins  | supplementary private insurance Yes/No   | binary  |
#  | totchr   | number of chronic problems (0-7)         | num     |
#  | age      | age (sample: 65-90)                      | integer |
#  | female   | gender dummy (F=1, M=0)                  | binary  |
#
#
#
#
# Data illustration
ggplot(dta, aes(x=age,y=ltotexp) ) + 
  geom_point() +
  geom_smooth(method= "lm") +
  facet_grid(~female) +
  ggtitle("Medical expenditures ~ age (by gender) using OLS") 
#
## Bonus question:
# Are you familiar with ggplot2? Can you produce another 
# illustrative & relevant plot using ggplot?

ggplot(dta, aes(x = totchr, y = ltotexp)) +
  geom_point(position = position_jitter(width = 0.1), alpha=0.5) + 
  geom_smooth(method= "lm")+
  facet_grid(~female)+
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") + 
  labs(title = "Medical expenditures~ Number of Chronic Problems (by gender) ",
       x = "Gender (0 = Male, 1 = Female)",
       y = "Log of Total Expenditures")+ theme_minimal()

# Plot illustrates variation of chronic problems by gender and showcases how
# log total expenditures grow with an increasing number of diseases


## TASK 1
# Perform OLS regression of the model (1) & show summary results:
# provide interpretation for at least two estimated coefficients

# model:

# ltotexp <- 6.1654 + 0.2622 * suppins + 0.4470 * totchr + 0.0131 * age − 0.0739 * female + ϵ  

# Response:

# Written representation of the model is ltotexp=β0*β1*×suppins+β2*×totchr+β3*×age+β4*×female+ϵ
model<-lm(ltotexp~ . , dta)
# View the summary of the model
summary(model)
# Adjust a scientific format of the output
options(scipen=10)

# By Performing OLS regression results are: 
# Intercept is 6.165
# Min. standard error belongs to age, meaning that estimation is the most precise for a given regressor
# Max. standard error belongs to Intercept and to suppins, meaning that their estimation is least precise
# t value of the intercept=22.757 and totchr=25.478 has the stronger evidence against
# the null hypothesis (that the coefficient is zero)
# all regressors except female have p-value converging to zero, which indicate
# significant association of confounders with the target variable
# suppins - change in 1 pp will increase medical expenditures by 0.26221
# totchr - change in 1 pp will increase medical expenditures by 0.44698
# age - change in 1 pp will increase medical expenditures by 0.01312


## TASK 2
# Use the same model, perform LAD (QREG for tau=0.5) and show summary results
# provide interpretation for at least two estimated coefficients

# Response:

# Perform LAD regression
model_lad <- rq(ltotexp ~ suppins + totchr + age + female, data = dta, tau = 0.5)

# View the summary of the model
summary(model_lad)

# Intercept value is 6.04940
# suppins - coefficient is 0.28280 indicates that having supplementary private insuarance
# is associated with the growth of the log medical expenditure at the median.
# It could be defined as people with supp.medical insurance are prone to access
# medical services 0.2820 times more.

# totchr - coefficient is 0.39099 claims that each auxiliary chronic condition
# of individual will evoke the growth by 0.39099 in the log of medical expenditures
# in the median (ceteris paribus). In other words, it can be interpreted as people
# with chronic diseases are tend to incur higher medical expenses.

# age - coefficient is 0.01601 indicates about tendency to increase log medical expenditures
# by 0.01601 in the median (ceteris paribus) with a 1 pp increase in a age. In other 
# words, individuals are prone to incur bigger medical expenses with an increasing age.

# female - coefficient is -0.07704 indicates that expenditures of females
# are smaller by 0.07704 in median (ceteris paribus) then males. It communicates
# the phenomena that males are tend to incur higher medical expenditures then females.

## TASK 3
# Perform QREG for tau= 0.1, 0.2, .... , 0.9
# plot the estimated parameters against different tau levels,
# compare the plotted qreg parameters with OLS estimates
# Comment on results you find interesting.

# Response:

# Quantile regression
model_qreq <- rq(ltotexp ~ suppins + totchr + age + female,  dta, 
                 tau = seq(0.1, 0.9, by = 0.1))
# View the summary 
summary(model_qreq)
# Visualization
plot(summary(model_qreq, se="boot"), ols=T)
# Comparison with OLS
coeftest(model)

# totchr - ols underestimate the model in quantile 0.1 and overestimate the model
# in 0.5:0.9 quantiles
# wide confidence intervals for age and female communicate more uncertainty
# about the estimates at quantiles

## TASK 4
# For alpha = 0.05, test the null hypothesis of equal coefficients at tau=0.3 and tau=0.5
# (use the test statistic as shown in seminar)
# interpret the test results (as comments in the script - use `#`)

# Response:

# ANOVA test for equal coefficients at tau 0.3 and 0.5
q3 <- rq(ltotexp ~ suppins + totchr + age + female, tau = 0.3, data=dta, alpha=0.05)
q5 <- rq(ltotexp ~ suppins + totchr + age + female, tau = 0.5, data=dta, alpha=0.05)
anova(q3, q5)

# p-value of 0.01398 suggests that the influence of regressors on the target variable's
# conditional quantiles varies between given points in the distribution.
# Subsequently we reject H0 about equal slopes.
# F-test indicates that the relationship at least between one predictor changes between
# 3rd and 5th decile. Meaning that the effect of confounders on log of total
# expenditure isn't consistent.
