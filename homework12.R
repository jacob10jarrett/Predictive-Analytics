############################## 
# Name: Jacob Jarrett
# Homework #12 
##############################

#================================================================================
# Slide 16: Cars dataset tasks
#================================================================================
library(MASS)

# 1. First six observations
head(cars, 6)

# 2. Scatter plot: speed vs. dist
plot(cars$speed, cars$dist,
     main = "Slide 16: Speed vs. Distance",
     xlab = "Speed", ylab = "Distance")

# 3. Add density contours to scatter
dens <- kde2d(cars$speed, cars$dist, n = 50)
contour(dens, add = TRUE)

# 4. Two boxplots side-by-side
par(mfrow = c(1, 2))
boxplot(cars$speed, main = "Speed", ylab = "Speed")
boxplot(cars$dist,  main = "Distance", ylab = "Distance")
par(mfrow = c(1, 1))

# 5. Density plots with skewness
skewness <- function(x) {
  m3 <- sum((x - mean(x))^3) / length(x)
  s3 <- (sqrt(var(x)))^3
  m3 / s3
}
par(mfrow = c(1, 2))
spd_den <- density(cars$speed)
plot(spd_den, main = sprintf("Speed Density\nskew=%.3f", skewness(cars$speed)))
polygon(spd_den, col = rgb(0, 0, 1, 0.2))
dist_den <- density(cars$dist)
plot(dist_den, main = sprintf("Distance Density\nskew=%.3f", skewness(cars$dist)))
polygon(dist_den, col = rgb(1, 0, 0, 0.2))
par(mfrow = c(1, 1))

# 6. Correlation
cor(cars$speed, cars$dist)

# 7. Linear model: dist ~ speed
model_cars <- lm(dist ~ speed, data = cars)

# 8. ANOVA table
anova(model_cars)

# 9. Conclusion
cat("Slide 16: Positive, significant relationship; stopping distance increases with speed.\n\n")

#================================================================================
# Slide 17: Marketing budget dataset tasks
#================================================================================
dataset <- read.csv("data-marketing-budget-12mo.csv", stringsAsFactors = FALSE)

# 1. First six records
head(dataset, 6)

# 2. Boxplots: spend & sales side-by-side
par(mfrow = c(1, 2))
boxplot(dataset$spend, main = "Slide 17: Spend", ylab = "Spend")
boxplot(dataset$sales, main = "Slide 17: Sales", ylab = "Sales")
par(mfrow = c(1, 1))

# 3. Interpretation: visual relationship
# Higher spend appears associated with higher median sales.

# 4. Scatterplot: spend vs. sales
plot(dataset$spend, dataset$sales,
     main = "Slide 17: Spend vs. Sales",
     xlab = "Spend", ylab = "Sales")

# 5. Linear model: sales ~ spend
model_marketing <- lm(sales ~ spend, data = dataset)

# 6. Model diagnostics
par(mfrow = c(2, 2))
plot(model_marketing)
par(mfrow = c(1, 1))

# 7. Residual analysis
# Residuals appear randomly scattered without funnel shape ⇒ homoscedasticity.

# 8. Summary statistics
summary(model_marketing)

# 9. Interpretation of regression output
# Positive, significant slope (p < 0.05) ⇒ spend is a significant predictor of sales.

# 10. Conclusions
cat("Slide 17: Spend significantly predicts sales (positive relationship).\n\n")

#================================================================================
# Slide 18: Salary data setup
#================================================================================
salary.table <- read.csv("Salary_Data.csv", stringsAsFactors = FALSE)
salary.table$E <- factor(salary.table$E)
salary.table$M <- factor(salary.table$M)

# Preview
head(salary.table)

#================================================================================
# Slide 19: Salary modeling tasks
#================================================================================
# 1. Model: Salary ~ Experience + Education + Management
model_sal <- lm(S ~ X + E + M, data = salary.table)

# 2. Residual plots (histogram & Q-Q)
par(mfrow = c(1, 2))
hist(resid(model_sal), main = "Slide 19: Residuals", xlab = "Residuals")
qqnorm(resid(model_sal), main = "Slide 19: Q-Q Plot"); qqline(resid(model_sal))
par(mfrow = c(1, 1))

# 3. Normality: Q-Q straight line indicates approximate normality.

# 4. Summary statistics
sum_sal <- summary(model_sal)
sum_sal

# 5. Smallest p-values
coef_tab <- sum_sal$coefficients
pvals <- coef_tab[, 4]
names(sort(pvals))[1:2]

# 6. Smallest standard errors
ses <- coef_tab[, 2]
names(sort(ses))[1:2]

# 7. ANOVA table
anova(model_sal)

# 8. Interpretation: predictors with smallest p-values are most influential.

# 9. Add interaction X:E
model_sal_int <- lm(S ~ X * E + M, data = salary.table)

# 10. Compare models
anova_comparison <- anova(model_sal, model_sal_int)
anova_comparison
cat("Slide 19: Interaction term significantly ", 
    ifelse(anova_comparison$Pr(>F)[2] < 0.05, "improves", "does not improve"),
    " model fit.\n\n", sep = "")

#================================================================================
# Slide 34: Box–Cox on savings dataset
#================================================================================
savings <- read.csv("savings.csv", stringsAsFactors = FALSE)
savings_model <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)

# 1. Box-Cox transformation
bc_sav <- boxcox(savings_model, lambda = seq(0.5, 1.5, 0.1))
opt_lambda <- bc_sav$x[which.max(bc_sav$y)]
opt_ll     <- max(bc_sav$y)
cat(sprintf("Slide 34: Optimal lambda = %.2f; max log-likelihood = %.3f\n", opt_lambda, opt_ll))

# 2. Shapiro-Wilk test on sr
shap_p <- shapiro.test(savings$sr)$p.value
cat(sprintf("Slide 34: Shapiro-Wilk p-value for sr = %.4f\n\n", shap_p))

#================================================================================
# Slide 35: Residual normality check for savings model
#================================================================================
par(mfrow = c(1, 2))
hist(resid(savings_model), main = "Slide 35: Residuals Hist", xlab = "Residuals")
qqnorm(resid(savings_model), main = "Slide 35: Q-Q Plot"); qqline(resid(savings_model))
par(mfrow = c(1, 1))
cat("Slide 35: Residuals approx normal (histogram symmetric, Q-Q roughly linear).\n\n")

#================================================================================
# Slide 46: mcheck and Box-Cox for cars; T/F question
#================================================================================
# 1. (T/F) A linear model is NOT linear in the parameters but in the random variables.
cat("Slide 46-1: FALSE (linear model is linear in parameters)\n")

# mcheck function
mcheck <- function(obj) {
  rs <- obj$residuals
  fv <- obj$fitted.values
  par(mfrow = c(1, 2))
  plot(fv, rs, main = paste("Residuals vs Fitted -", deparse(substitute(obj))),
       xlab = "Fitted", ylab = "Residuals"); abline(h = 0, lty = 2)
  qqnorm(rs, main = "Q-Q Plot"); qqline(rs)
  par(mfrow = c(1, 1))
}

# 2. Investigate distributions
set.seed(123)
x_b <- rbeta(1000, 2, 3);    mcheck(lm(x_b ~ 1))
x_w <- rweibull(1000, 2, 4);  mcheck(lm(x_w ~ 1))
x_l <- rlogis(1000, 3, 2);    mcheck(lm(x_l ~ 1))

# 3. Box-Cox on cars model
bc_cars <- boxcox(model_cars, lambda = seq(-2, 2, 0.1))
opt_lam_c <- bc_cars$x[which.max(bc_cars$y)]
opt_ll_c  <- max(bc_cars$y)
cat(sprintf("Slide 46-3: Optimal lambda = %.2f; max log-likelihood = %.3f\n\n", opt_lam_c, opt_ll_c))

#================================================================================
# Slide 53: Influence measures for key models
#================================================================================
cd1 <- cooks.distance(model_cars)
cat("Slide 53: Cars model max Cook's D obs =", which.max(cd1), 
    "value =", round(max(cd1), 4), "\n")
cd2 <- cooks.distance(model_marketing)
cat("Slide 53: Marketing model max Cook's D obs =", which.max(cd2), 
    "value =", round(max(cd2), 4), "\n")
cd3 <- cooks.distance(model_sal)
cat("Slide 53: Salary model max Cook's D obs =", which.max(cd3), 
    "value =", round(max(cd3), 4), "\n")
cd4 <- cooks.distance(savings_model)
cat("Slide 53: Savings model max Cook's D obs =", which.max(cd4), 
    "value =", round(max(cd4), 4), "\n\n")

#================================================================================
# Slide 71: AIC for key models
#================================================================================
cat("Slide 71: AIC values:\n",
    "Cars model:       ", AIC(model_cars), "\n",
    "Marketing model:  ", AIC(model_marketing), "\n",
    "Salary model:     ", AIC(model_sal), "\n",
    "Savings model:    ", AIC(savings_model), "\n\n", sep = "")

#================================================================================
# Slide 87: Extract summary elements [[1]] through [[11]]
#================================================================================
for (m in list(model_cars, model_marketing, model_sal, savings_model)) {
  sm <- summary(m)
  cat("\nElements of summary for model:", deparse(substitute(m)), "\n")
  for (i in 1:11) {
    cat(sprintf("[[%d]]:\n", i))
    print(sm[[i]])
  }
}
cat("\n")

#================================================================================
# Slides 102 & 103: Stepwise deletion on toxicity data
#================================================================================
url2 <- "http://pages.stat.wisc.edu/~ane/st572/data/toxic.txt"
Toxicity.Data <- read.table(url2, header = TRUE)

fit1 <- lm(toxicity ~ 1, data = Toxicity.Data)
fit2 <- lm(toxicity ~ dose, data = Toxicity.Data)
fit3 <- lm(toxicity ~ weight, data = Toxicity.Data)
fit4 <- lm(toxicity ~ dose + weight, data = Toxicity.Data)

# Compare models
anova(fit1, fit2)
anova(fit1, fit3)
anova(fit1, fit4)
anova(fit2, fit4)

# Conclusion
cat("Slides 102/103: Best model is fit4 with both dose and weight (largest deviance reduction and significant p-values).\n")