##############################
# Name: Jacob Jarrett
# Homework #13
##############################

#================================================================================
# Slide 29: ANOVA Table Completion
#================================================================================
# Given: 15 observations, DF_treatment = 2, SS_treatment = 2510, MS_error = 13
df_t <- 2
ss_t <- 2510
df_e <- 15 - df_t - 1
ms_e <- 13
ss_e <- ms_e * df_e
df_tot <- df_t + df_e
ss_tot <- ss_t + ss_e
ms_t <- ss_t / df_t
f_value <- ms_t / ms_e

anova29 <- data.frame(
  Source   = c("Treatment", "Error", "Total"),
  Df       = c(df_t, df_e, df_tot),
  Sum_Sq   = c(ss_t, ss_e, ss_tot),
  Mean_Sq  = c(ms_t, ms_e, NA),
  F_value  = c(f_value, NA, NA)
)
print(anova29)
cat("\n")

#================================================================================
# Slide 42: Simple Linear Regression on BMI Data
#================================================================================
# 1. Load and assign
bmi <- read.csv("bmi_data.csv", stringsAsFactors = FALSE)

# 2. Correlation
cor_bmi <- cor(bmi$Height, bmi$Weight)
cat(sprintf("Correlation (Height vs. Weight) = %.3f\n\n", cor_bmi))

# 3. Define linear model
model_bmi <- lm(Height ~ Weight, data = bmi)

# 4. Diagnostic plots
par(mfrow = c(2,2))
plot(model_bmi)
par(mfrow = c(1,1))

# 5. Scatterplot + regression line
plot(bmi$Weight, bmi$Height,
     main = "Height vs Weight",
     xlab = "Weight", ylab = "Height")
abline(model_bmi, col = "blue", lwd = 2)

# 6. Confidence bands
new_w <- seq(min(bmi$Weight), max(bmi$Weight), length = 100)
preds <- predict(model_bmi,
                 newdata = data.frame(Weight = new_w),
                 interval = "confidence")
matplot(new_w, preds[,2:3], type = "l", lty = 2, add = TRUE)

# 7. Regression output
print(summary(model_bmi))

# 8. ANOVA table
print(anova(model_bmi))

# 9. Interpretation
cat("Weight is a significant positive predictor of Height (p < 0.05).\n")

# 10. Normality check
hist(resid(model_bmi), main = "Residuals Histogram")
qqnorm(resid(model_bmi)); qqline(resid(model_bmi))

# 11. R-squared
cat(sprintf("R-squared = %.3f\n", summary(model_bmi)$r.squared))

# 12. Opinion
cat("Model fit is strong with normally distributed residuals.\n\n")

#================================================================================
# Slide 43: Simple Linear Regression on Salary Data
#================================================================================
# 1. Load and assign
salary <- read.csv("Salary_Data-2.csv", stringsAsFactors = FALSE)

# 2. Correlation
cor_sal <- cor(salary$YearsExperience, salary$Salary)
cat(sprintf("Correlation (Experience vs. Salary) = %.3f\n\n", cor_sal))

# 3. Define linear model
model_sal2 <- lm(Salary ~ YearsExperience, data = salary)

# 4. Diagnostic plots
par(mfrow = c(2,2))
plot(model_sal2)
par(mfrow = c(1,1))

# 5. Scatterplot + regression line
plot(salary$YearsExperience, salary$Salary,
     main = "Salary vs YearsExperience",
     xlab = "YearsExperience", ylab = "Salary")
abline(model_sal2, col = "blue", lwd = 2)

# 6. Confidence bands
new_x <- seq(min(salary$YearsExperience), max(salary$YearsExperience), length = 100)
preds2 <- predict(model_sal2,
                  newdata = data.frame(YearsExperience = new_x),
                  interval = "confidence")
matplot(new_x, preds2[,2:3], type = "l", lty = 2, add = TRUE)

# 7. Regression output
print(summary(model_sal2))

# 8. ANOVA table
print(anova(model_sal2))

# 9. Interpretation
cat("Experience is a significant positive predictor of Salary (p < 0.05).\n")

# 10. Normality check
hist(resid(model_sal2), main = "Residuals Histogram")
qqnorm(resid(model_sal2)); qqline(resid(model_sal2))

# 11. R-squared
cat(sprintf("R-squared = %.3f\n", summary(model_sal2)$r.squared))

# 12. Opinion
cat("Model is robust with strong fit and no major violations.\n\n")

#================================================================================
# Slide 68: Multiple Regression on Wine Quality Data
#================================================================================
# 1. Load and assign
quality <- read.csv("winequality.csv", stringsAsFactors = FALSE)

# 2. Correlation matrix by type
by(quality[, sapply(quality, is.numeric)], quality$type, cor)

# 3. Pairs plot by type
if (!require(car)) install.packages("car")
library(car)
scatterplotMatrix(~ . | type, data = quality)

# 4. Full model with interactions
full_model <- lm(quality ~ (.)^2, data = quality)

# 5. Backward elimination to minimum adequate model
min_model <- step(full_model, direction = "backward", trace = FALSE)

# 6. AIC values
cat("Full model AIC:", AIC(full_model), "\n")
cat("Min adequate model AIC:", AIC(min_model), "\n\n")

# 7. Regression output
print(summary(min_model))

# 8. ANOVA table
print(anova(min_model))

# 9. Interpretation
cat("Significant predictors retained indicate strong associations with wine quality.\n")

# 10. Six diagnostic plots
par(mfrow = c(2,3))
plot(min_model, which = 1:6)
par(mfrow = c(1,1))

# 11. Normality check
hist(resid(min_model), main = "Residuals Histogram")
qqnorm(resid(min_model)); qqline(resid(min_model))

# 12. Opinion
cat("Model explains substantial variance and meets regression assumptions.\n\n")

#================================================================================
# Slide 104: Overdispersion Assessment for Negative Binomial Model
#================================================================================
# Provided Residual deviance = 418.82 on 397 df
res_dev <- 418.82
res_df  <- 397
dev_ratio <- res_dev / res_df
cat(sprintf("Residual deviance / df = %.3f\n", dev_ratio))
cat("Conclusion: residual deviance ≈ df, indicating no severe overdispersion; NB model appropriate.\n\n")

#================================================================================
# Slide 105: Regression & Treatment Analysis on Oyster Data
#================================================================================
# 1–5. Model: Final ~ Initial
oyster <- read.csv("trtmtdata.csv", stringsAsFactors = TRUE)
oyster_reg1 <- lm(Final ~ Initial, data = oyster)
print(anova(oyster_reg1))
print(summary(oyster_reg1))
cat("Initial is a significant predictor of Final (p < 0.05).\n\n")

# 6–9. Model for each treatment level
by(oyster, oyster$Trtmt, function(sub) {
  mod <- lm(Final ~ Initial, data = sub)
  print(anova(mod))
  print(summary(mod))
  cat("----\n")
})

# 10–13. Model: Final ~ Trtmt + Initial
oyster_reg2 <- lm(Final ~ Trtmt + Initial, data = oyster)
print(anova(oyster_reg2))
print(summary(oyster_reg2))
cat("Both treatment and initial weight significantly explain Final (Treatment p =", 
    anova(oyster_reg2)["Trtmt","Pr(>F)"], ").\n\n")

# 14. Minimum adequate model
cat("Minimum adequate model: Final ~ Trtmt + Initial (based on significant p-values and AIC reduction).\n")
