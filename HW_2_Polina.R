#  HW 2, based on:
#
#  Greene, Econometric analysis, 7th ed., Example 6.9, p. 212
#  "Structural break in the gasoline market"
#
#  Script Outline:
#  - Data description
#  - Read the dataset & basic transform
#  - HW 2
#
#
# Data description
# see also http://pages.stern.nyu.edu/~wgreene/Text/Edition7/tablelist8new.htm
#
# The U.S. Gasoline Market, 52 Yearly Observations, 1953-2004
#
# Year = Year, 1953-2004,
# GasExp = Total U.S. gasoline expenditure (this is the PG variable),
# Pop = U.S. total population in thousands
# GasP = Price index for gasoline (this is the PG variable),
# Income = Per capita disposable income (calculated as "Income"/Pop),
# Pnc = Price index for new cars,
# Puc = Price index for used cars,
# Ppt = Price index for public transportation,
# Pd = Aggregate price index for consumer durables,
# Pn = Aggregate price index for consumer nondurables,
# Ps = Aggregate price index for consumer services.
#
#
# Read the dataset
directory <- setwd("/Users/polinavlasova/Desktop/Econometrics")
HW2dat <- read.csv("TableF2-2.csv")
# add time trend variable
HW2dat$trend <- seq(1:52)
# produce dependent variable
HW2dat$depvar <- log(HW2dat$GASEXP/HW2dat$POP)
# 1st regressor (INCOME) is already per capita in your dataset...
# log-transformed regressors can be included directly into 
# lm() function, eg as: log(INCOME)
#
#
# HW 2:
# 1) reproduce estimation columns 1,3,4 in Table 6.7
#    .. by providing output from lm() function
#    .. you don't have to format the output into a table.
#
#    .. for the 3rd and 4th column of table 6.7 in Example 6.9,
#    .. use subset argument in the lm() function
#    .. for syntax, see ?lm or
#    .. https://stackoverflow.com/questions/33113544/how-to-subset-a-range-of-values-in-lm

# 1953-2004
HW2dat$GPOP <- (HW2dat$GASEXP) / (HW2dat$GASP*HW2dat$POP)
HW2dat <- ts(HW2dat, start = 1953, frequency = 1)
model <- lm(log(GPOP)~log(INCOME)+log(GASP)+log(PNC)+log(PUC)+trend,HW2dat)
summary(model)

#Preshock model (1953-1973)
preshock_model <- lm(log(GPOP) ~ log(INCOME)+log(GASP)+log(PNC)+log(PUC)+trend, 
                     data=HW2dat, 
                     subset=(YEAR <= 1973))
summary(preshock_model)

# Postshock period (1974-2004)
postshock_model <- lm(log(GPOP) ~ log(INCOME)+log(GASP)+log(PNC)+log(PUC)+trend, 
                      data=HW2dat, 
                      subset=(YEAR >= 1974))
summary(postshock_model)

# The coefficients provide insights into how various factors impact gasoline consumption. 
# The differences between the preshock and postshock periods suggest that the 'shock' event had a substantial
# effect on these relationships. The models are statistically robust, as indicated by the 
# high R^2 values, suggesting they fit the data well. Income per capita has a consistent positive effect on gasoline 
# consumption across all models, indicating that as per capita income increases, 
# so does gasoline consumption. Negative coefficients for PG,PNC and PUC suggest
# that with the price growth gasoline consumtion tends to decline.

# 2) test for structural break in 1973
library(gap)

y <- log(HW2dat[, "GPOP"])
x <- model.matrix(~ YEAR + log(INCOME) + log(GASP), data=HW2dat)[,-1] 

start_year <- start(HW2dat)[1]
split_point <- 1973 - start_year + 1

y1 <- y[1:split_point]
y2 <- y[(split_point+1):length(y)]
x1 <- x[1:split_point,]
x2 <- x[(split_point+1):nrow(x),]

chow.test(y1,x1,y2,x2)

# The Chow Test results indicate a significant structural break in the year 1973, 
# with an F-statistic of about 7.65 and an extremely small p-value (3.09e-19). 
# This represents that the model’s parameters before and after 1973 are significantly different, 
# corroborating the presence of a structural change in a given year.

# 3) use the example in 'Chow_test.R' to test for structural
#    break over the observed period

sctest(lm(log(GPOP) ~ YEAR + log(INCOME) + log(GASP), data=HW2dat), type = "Chow", points=53)

# p-value of 0.003937, which is below the typical significance threshold of 0.05, 
# there is statistical evidence to suggest that there is noticeable fluctuation in the
# model parameters over time. Relationships between target variable and predictors
# are re not stable throughout the entire time span of the data.

FS <- Fstats(log(GPOP) ~ YEAR + log(INCOME) + log(GASP), data=HW2dat, from = 5, to = 47)
FS$Fstats   
plot(FS)
plot(FS, pval = T)

# The p-values are very low (close to zero) for a significant portion of the time series, 
# which suggests that the null hypothesis of no structural break can be strongly rejected for many points in time. 
# This indicates that there are significant structural changes at these points.
# Starting from 1990 p-value commenced to grow dramatically with a slight
# decrease in the end of nineties followed by drastic increase and reaching its
# maximum in 2004.
# All p-values are below 0.05 significance level suggesting that there are some points 
# in time series where the regression model changes significantly and we reject the
# hypothesis about model stability.
# Plot of F-statistics represents visually structural change for 1973
# where F reached its maximum. After 1973 critical value commenced to drop.
# It confirms that gasoline market changed in a given year leading to
# overall instability of the model.
# 4) Please make sure to comment/interpret your results & test results.


