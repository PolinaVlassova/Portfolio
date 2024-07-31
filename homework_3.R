directory <- setwd("/Users/polinavlasova/Downloads")
p_values <- read.csv("p_values.csv", sep=" ", header=TRUE)

#1


library(ggplot2)

plot_p_values_corrections <- function(pvalues, m = 78, alpha = 0.05) {
  # Calculate indices of hypotheses
  j <- 1:m
  
  # Calculate correction thresholds
  Bon <- rep(alpha / m, m)  # Bonferroni threshold is constant across all tests
  Sidak <- rep(1 - (1 - alpha)^(1 / m), m)  # Sidak threshold is constant
  Holm <- alpha / (m + 1 - j)  # Holm threshold varies
  BH <- alpha * j / m  # Benjamini-Hochberg threshold varies
  
  # Sort p-values for plotting
  sorted_pvalues <- sort(pvalues)
  
  # Create the plot
  plot <- ggplot(data = data.frame(j = j, Bon = Bon, Sidak = Sidak, Holm = Holm, BH = BH, pvalues = sorted_pvalues)) +
    geom_line(aes(x = j, y = alpha, col = "Unadjusted = 0.05"), lwd = 2) +
    geom_line(aes(x = j, y = Bon, col = "Bonferroni = 0.0025"), lwd = 2) +
    geom_line(aes(x = j, y = Sidak, col = "Sidak´s = 0.00256"), lwd = 2) +
    geom_line(aes(x = j, y = Holm, col = "Holm"), lwd = 2) +
    geom_line(aes(x = j, y = BH, col = "Benjamini-Hochberg"), lwd = 2) +
    geom_point(aes(x = j, y = pvalues, col = "p-values"), size = 3) +
    scale_x_continuous("Index of Hypothesis (j)", limits = c(0, m)) + 
    scale_y_continuous("p-values and Correction Thresholds", limits = c(0, 0.1)) +
    labs(colour = "Legend") +
    theme_minimal()
  
  # Print the plot
  return(plot)
}
plot_p_values_corrections(p_values$V2)
plot_p_values_corrections(p_values$V3)
plot_p_values_corrections(p_values$V4)
plot_p_values_corrections(p_values$V5)
plot_p_values_corrections(p_values$V6)


# V2 group: 10 hypotheses appear to be significant under the more conservative methods (Bonferroni, Šidák, and Holm).
# 15 hypotheses are significant under the less conservative Benjamini-Hochberg method.

# V3: 18 hypotheses appear to be significant under the more conservative methods (Bonferroni, Šidák, and Holm).
# 30 hypotheses are significant under the less conservative Benjamini-Hochberg method.

# V4: 5 hypotheses appear to be significant under the more conservative methods (Bonferroni, Šidák, and Holm).
# 13 hypotheses are significant under the less conservative Benjamini-Hochberg method.

# V5: 4 hypotheses appear to be significant under the more conservative methods (Bonferroni, Šidák, and Holm).
# 10 hypotheses are significant under the less conservative Benjamini-Hochberg method.

# V6: 18 hypotheses appear to be significant under the more conservative methods (Bonferroni, Šidák, and Holm).
# 26 hypotheses are significant under the less conservative Benjamini-Hochberg method.

# What does cause the rejection of hypothesis? It seems that rejection of 
# hypothesis happened due to small number of tests. With an increasing number of tests
# p-value doesn't fall into rejection region.


#3 
set.seed(7)
n <- 200

# Set the probability of success (proportion of 1's)
p <- runif(1, min = 0.3, max = 0.7)

# Simulate the binary variable using rbinom
X <- rbinom(n, size = 1, prob = p)


#4
# Normal distribution variables (mean = 0, standard deviation = 1)
Y1 <- rnorm(n, mean = 0, sd = 1)
Y2 <- rnorm(n, mean = 0, sd = 2)
Y3 <- rnorm(n, mean = 5, sd = 1)
# Gamma distribution variables (shape = k, rate = theta)
Y4 <- rgamma(n, shape = 2, rate = 1)
Y5 <- rgamma(n, shape = 2, rate = 0.5)
Y6 <- rgamma(n, shape = 5, rate = 1)
# Exponential distribution variables (rate = 1/mean)
Y7 <- rexp(n, rate = 1)  # Mean = 1
Y8 <- rexp(n, rate = 0.5)  # Mean = 2
Y9 <- rexp(n, rate = 2)  # Mean = 0.5
Y10 <- rnorm(n, mean = 10, sd = 3)
Y11 <- rnorm(n, mean = -3, sd = 0.5)
Y12 <- rgamma(n, shape = 9, rate = 3)
Y13 <- rgamma(n, shape = 7, rate = 2)
Y14 <- rexp(n, rate = 0.2)  # Mean = 5
Y15 <- rexp(n, rate = 3)  # Mean = 1/3
Y16 <- rnorm(n, mean = 3, sd = 5)
Y17 <- rnorm(n, mean = 0, sd = 10)

# 5
# Assuming you have already generated X
n <- length(X)

# Initialize the variables
Y18 <- numeric(n)
Y19 <- numeric(n)
Y20 <- numeric(n)

# Populate Y18, Y19, Y20 based on the value of X
for (i in 1:n) {
  if (X[i] == 0) {
    Y18[i] <- rnorm(1, mean = 0, sd = 1)
    Y19[i] <- rgamma(1, shape = 1, rate = 1/20)  # using rate as the inverse of scale (theta)
    Y20[i] <- rexp(1, rate = 1.5)
  } else {
    Y18[i] <- rnorm(1, mean = 0.5, sd = 1)
    Y19[i] <- rgamma(1, shape = 1.5, rate = 1/20)
    Y20[i] <- rexp(1, rate = 2)
  }
}

# 6
perform_welchs_t_tests <- function(X, var_prefix, num_vars) {
  p_values <- numeric(num_vars)
  for (i in 1:num_vars) {
    var_name <- paste(var_prefix, i, sep = "")
    formula <- as.formula(paste(var_name, "~ X"))
    test_result <- t.test(formula, data = list(X = X, Y = eval(as.name(var_name))))
    p_values[i] <- test_result$p.value
  }
  return(p_values)
}
p_values <- perform_welchs_t_tests(X, "Y", 20)
print(p_values)

