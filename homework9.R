##############################
# Name: Jacob Jarrett
# Homework #9
##############################

#================================================================================
### === SLIDE 48 ===  
#================================================================================

# Question 1: 95th percentile of IQ scores (mean = 100, sd = 15)
qnorm(0.95, mean = 100, sd = 15)

# Question 2:
# 1. Generate 1000 IID normal random numbers
set.seed(123)
iq_data <- rnorm(1000, mean = 100, sd = 15)

# 2. Plot histogram
hist(iq_data, breaks = 30, col = "lightblue", main = "Histogram of IQ Scores", xlab = "IQ")

# 3 & 4. Plot the probability density function (PDF)
x_vals <- seq(40, 160, length.out = 500)
y_vals <- dnorm(x_vals, mean = 100, sd = 15)
lines(x_vals, y_vals * length(iq_data) * diff(hist(iq_data, plot = FALSE)$breaks)[1], col = "red", lwd = 2)

#================================================================================
### === SLIDE 57 ===  
#================================================================================

# Repeat example using Poisson distribution with lambda = 25
set.seed(123)
pois_data <- rpois(1000, lambda = 25)
hist(pois_data, breaks = 20, probability = TRUE, main = "Poisson Histogram (λ = 25)", col = "lightgreen")

# Overlay Poisson PMF
x_vals <- seq(min(pois_data), max(pois_data), by = 1)
pois_pmf <- dpois(x_vals, lambda = 25)
lines(x_vals, pois_pmf, col = "blue", lwd = 2)

#================================================================================
### === SLIDE 81 ===  
#================================================================================

# Chi-squared probabilities
# 1. P(X < 2.34) for df = 6
pchisq(2.34, df = 6)

# 2. P(X > 15.34) for df = 9
1 - pchisq(15.34, df = 9)

# 3. P(X < 6.66 or X > 27.34) for df = 17
pchisq(6.66, df = 17) + (1 - pchisq(27.34, df = 17))

# 4. P(5.25 < X < 25.41) for df = 14
pchisq(25.41, df = 14) - pchisq(5.25, df = 14)

#================================================================================
### === SLIDE 82 ===  
#================================================================================

# Chi-squared x-scores
# 5. x such that P(X < x) = 0.0333, df = 5
qchisq(0.0333, df = 5)

# 6. x such that P(X > x) = 0.125, df = 25
qchisq(0.125, df = 25, lower.tail = FALSE)

# 7. x-scores for middle 75% with equal tails, df = 11
qchisq(c(0.125, 0.875), df = 11)

# 8. x-scores for outer 3.33% in tails, df = 23
qchisq(c(0.01665, 0.98335), df = 23)

#================================================================================
### === SLIDE 99 ===  
#================================================================================

# Find α such that gamma distribution with rate = 0.5 matches the graph
x_vals <- seq(0, 5, length.out = 500)
y_vals <- dgamma(x_vals, shape = 2, rate = 0.5)

plot(x_vals, y_vals, type = "l", col = "red", lwd = 2, main = "Gamma Distribution", xlab = "x", ylab = "y")

# Based on shape, alpha ≈ 2 fits the graph shown in slide 99