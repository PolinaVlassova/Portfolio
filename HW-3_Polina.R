# Home assignment - IVR example.
#
## Please follow instructions below:
#
library(AER)
#
############## Data and data description
#
data <- read.csv("schooling.csv")
#
# A data frame with 3010 rows and 22 columns.
# 
# wage
# Raw wages in 1976 (in cents per hour).
# 
# education
# Education in 1976 (in years).
# 
# experience
# Years of labor market experience, computed as age - education - 6.
# 
# ethnicity
# Factor indicating ethnicity. Is the individual African-American ("afam") or not ("other")?
#   
# smsa
# Factor. Does the individual reside in a SMSA (standard metropolitan statistical area) in 1976?
#   
# south
# Factor. Does the individual reside in the South in 1976?
#   
# age
# Age in 1976 (in years).
# 
# nearcollege
# Factor. Did the individual grow up near a 4-year college?
#   
# nearcollege2
# Factor. Did the individual grow up near a 2-year college?
#   
# nearcollege4
# Factor. Did the individual grow up near a 4-year public or private college?
#   
# enrolled
# Factor. Is the individual enrolled in college in 1976?
#   
# married
# factor. Is the individual married in 1976?
#   
# education66
# Education in 1966 (in years).
# 
# smsa66
# Factor. Does the individual reside in a SMSA in 1966?
#   
# south66
# Factor. Does the individual reside in the South in 1966?
#   
# feducation
# Father's educational attainment (in years). Imputed with average if missing.
# 
# meducation
# Mother's educational attainment (in years). Imputed with average if missing.
# 
# fameducation
# Ordered factor coding family education class (from 1 to 9).
# 
# kww
# Knowledge world of work (KWW) score.
# 
# iq
# Normed intelligence quotient (IQ) score
# 
# parents14
# Factor coding living with parents at age 14: both parents, single mother, step parent, other
# 
# library14
# Factor. Was there a library card in home at age 14?
#
#
############## Model description
#
# Use the following model specification:
#
#
#   log(wage) = beta_0   +   beta_1 * education   +   beta_2 * experience 
#               + beta_3 * experience^2   +   beta_4 * smsa   + error
#
#    - assume education can be endogenous
#    - use "meducation" and "feducation" as instruments for IVR estimation
#    - to include 2nd polynomial in the model, use: "+ poly(experience, 2, raw = TRUE)"
#
#
## Home Assignment tasks
#
# 1) Estimate model by OLS (here, you can assume education is exogenous)
# 2) Interpret beta_1 coefficient
# 3) Calculate and interpret the marginal effect of experience
# 4) Use the delta method to evaluate effect of experience, discuss results
# 5) Interpret the effect of smsa on wages
#
# IVR
#
# 6) Estimate model by ivreg
# 7) Interpret all diagnostic tests (separately first, than make final conclusion)
# 8) Interpret the coefficient on education, compare to OLS estimation
#
#
# To answer questions, provide both the necessary R code and your interpretations (as comments)
#
#
#
############## Provide solution below, use numbers 1-8 to identify answers.


directory <- setwd("/Users/polinavlasova/Downloads")
data <- read.csv("schooling.csv")

# 1) Estimate model by OLS (here, you can assume education is exogenous)
model <- lm(log(wage)~education+experience+I(experience^2)+smsa, data)
summary(model)

# 2) Interpret beta_1 coefficient
# each additional year of schooling increase the logged wage of individual by 0.088 pp.
# low p-value indicates significance of the estimate.

# 3) Calculate and interpret the marginal effect of experience
coefficients <- coef(model)
beta_2 <- coefficients["experience"]
beta_3 <- coefficients["I(experience^2)"]
mean_experience <- mean(data$experience)
# Marginal effect at different levels of experience: 5, 10 and 15 years
experience_levels <- c(5, 10, 15)
marginal_effects <- beta_2 + 2 * beta_3 * experience_levels
marginal_effects
marginal_effect_for_mean <- beta_2 + 2 * beta_3 *mean_experience
marginal_effect_for_mean
# For person with 5 years of experience each additional year of experience will increase
# logged wage by 0.06(6%). For a person with 10 years of experience growth will be 
# accounted for 0.04 (4%) to a logged wage. Individual with 15 years of experience
# could expect 0.017 (1.7%) growth of logged salary by additional year of experience.

# marginal effect of the regressor at the mean of  regressor, taking 
# into account the impact of both the linear and quadratic terms yields 0.04 pp increase in a logged wage
# would be triggered by a one unit growth of individual's experience

# 4) Use the delta method to evaluate effect of experience, discuss results
mean_experience <- mean(data$experience)
mean_experience
delta_result <- deltaMethod(model, "(experience + `I(experience^2)` * mean_experience)")
delta_result
# Estimate 0.067 represents the estimated marginal effect of one additional year of 
# experience on the logarithm of wage, evaluated at the mean level of experience.
# On average, each additional year of experience is associated with a 6.67151% increase in wages.

# 5) Interpret the effect of smsa on wages
# Estimate of 0.18 indicates that logged wage of individual on average increase by
# 0.18 pp(18%) when he resides in standard metropolitan statistical area. As it is categorical
# variable and "no" was considered as a threshold, non-residents of smsa encounter 0% increase. 

# 6) Estimate model by ivreg
library(AER)
iv_model <- ivreg(log(wage) ~ education + experience+I(experience^2) + smsa | 
                    meducation + feducation +experience+ I(experience^2) + smsa, data = data)
summary(iv_model)

# 7) Interpret all diagnostic tests (separately first, than make final conclusion)
summary(iv_model, vcov = sandwich, diagnostics = T)
# Weak Instruments:  low p-value leads to a rejection of null hypothesis as instruments are weak.
# This confirms the endogeneity of the regressors and confirms that the results should be different between each other
# and more consistent for the IVR model. 

# Wu-Hausman: A non-significant p-value  0.00000219 leads to the rejection of 
# null hypothesis about exogeneity of suspect variables, suggesting that the variable 
# education is indeed endogenously related to
# log(wage), and thus correctly treated as endogenous in  IV model.

# Sargan: we can't reject null hypothesis about non-correlation of IVs with error term.
# Significant P-value of 0.131 suggests that the instruments used in  model are likely valid
# without over-identification.

# 8) Interpret the coefficient on education, compare to OLS estimation

# Coefficient 0.137 in IVR model suggests that there is a 13% increase in a log wage
# with additional year of schooling. Low p-value indicate signifies strong and reliable
# effect, which is isolated from other variables.

# Coefficient 0.08 in OLS model suggests that there is a 8% increase in a log wage
# with additional year of schooling. In case of endogenous variables, IVR model 
# suggests more precise estimates due to endogeneity control. This suggests that 
# OLS may be underestimating the true causal effect of education on wages, potentially
# due to omitted variable bias where unobserved factors both affect wages and correlate 
# with education. Here, IVR demonstrates more robust estimates.
 