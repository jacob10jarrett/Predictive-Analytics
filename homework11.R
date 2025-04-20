##############################
# Name: Jacob Jarrett
# Homework #11 
##############################

#================================================================================
# Slide 12: Q–Q Plot of Mydata
#================================================================================
Mydata <- c(15, 10, 25, 37, 42, 12, 40, 38, 50, 44)
sorted_data <- sort(Mydata)
qqnorm(sorted_data, main = "Slide 12: Q–Q Plot of Mydata")
qqline(sorted_data)

#================================================================================
# Slide 21: Shapiro–Wilk Tests on Simulated Distributions
#================================================================================
# 1. (T/F) p = 0.03 ⇒ reject normality?
cat("Slide 21–1: p = 0.03 ⇒ reject normality? ", ifelse(0.03 < 0.05, "TRUE\n", "FALSE\n"))

set.seed(123)
# Beta(a=2,b=3)
x_beta <- rbeta(1000, 2, 3)
p_beta <- shapiro.test(x_beta)$p.value
# Weibull(α=2, λ=4)
x_weib <- rweibull(1000, 2, 4)
p_weib <- shapiro.test(x_weib)$p.value
# Logistic(μ=3, s=2)
x_logi <- rlogis(1000, 3, 2)
p_logi <- shapiro.test(x_logi)$p.value

cat(sprintf("Slide 21–2 (Beta):      p = %.4f ⇒ %s\n", 
            p_beta, ifelse(p_beta < 0.05, "reject normality", "cannot reject normality")))
cat(sprintf("Slide 21–2 (Weibull):   p = %.4f ⇒ %s\n", 
            p_weib, ifelse(p_weib < 0.05, "reject normality", "cannot reject normality")))
cat(sprintf("Slide 21–2 (Logistic):  p = %.4f ⇒ %s\n", 
            p_logi, ifelse(p_logi < 0.05, "reject normality", "cannot reject normality")))

#================================================================================
# Slide 31: Normality & Moments for cat data.csv
#================================================================================
cat_data <- read.csv("cat data.csv", stringsAsFactors = TRUE)

# 1–3. Histograms & Q–Q plots
hist(cat_data$x[cat_data$y=="No"],  main="Slide 31: Histogram x | y = No",  xlab="x")
hist(cat_data$x[cat_data$y=="Yes"], main="Slide 31: Histogram x | y = Yes", xlab="x")
par(mfrow=c(1,2))
qqnorm(cat_data$x[cat_data$y=="No"],  main="Q–Q: y=No");  qqline(cat_data$x[cat_data$y=="No"])
qqnorm(cat_data$x[cat_data$y=="Yes"], main="Q–Q: y=Yes"); qqline(cat_data$x[cat_data$y=="Yes"])
par(mfrow=c(1,1))
qqnorm(cat_data$x, main="Q–Q: Combined"); qqline(cat_data$x)

# 5. Shapiro–Wilk tests
sw_no  <- shapiro.test(cat_data$x[cat_data$y=="No"])$p.value
sw_yes <- shapiro.test(cat_data$x[cat_data$y=="Yes"])$p.value
sw_all <- shapiro.test(cat_data$x)$p.value
cat(sprintf("Slide 31: Shapiro p-values ⇒ No: %.4f, Yes: %.4f, All: %.4f\n",
            sw_no, sw_yes, sw_all))

# 6. Skewness & kurtosis functions
skew <- function(x) { m3 <- sum((x-mean(x))^3)/length(x); s3 <- sqrt(var(x))^3; m3/s3 }
kurtosis <- function(x) { m4 <- sum((x-mean(x))^4)/length(x); s4 <- var(x)^2; m4/s4 - 3 }

for(g in c("No","Yes","All")) {
  vals <- if(g=="All") cat_data$x else cat_data$x[cat_data$y==g]
  sk <- skew(vals); kt <- kurtosis(vals)
  t_sk <- sk/sqrt(6/length(vals))
  p_sk <- 2*(1 - pt(abs(t_sk), length(vals)-1))
  cat(sprintf("Slide 31: %s ⇒ skew=%.3f, kurt=%.3f, skew p=%.4f\n", 
              g, sk, kt, p_sk))
}

#================================================================================
# Slide 32: Exploratory & Normality for my_data
#================================================================================
set.seed(1234)
my_data <- data.frame(name=paste0("M_",1:10),
                      weight=round(rnorm(10,20,2),1))
print(head(my_data, 10))
print(summary(my_data))
boxplot(weight~1, data=my_data, main="Slide 32: Boxplot of Weight", ylab="Weight")
shap_p <- shapiro.test(my_data$weight)$p.value
cat(sprintf("Slide 32: Shapiro-Wilk p=%.4f\n", shap_p))
qqnorm(my_data$weight); qqline(my_data$weight)

#================================================================================
# Slide 68: Paired Pre vs Post (scores.csv)
#================================================================================
scores <- read.csv("scores.csv")
boxplot(scores$pre.score, scores$post.score, names=c("Pre","Post"),
        main="Slide 68: Pre vs Post Scores", ylab="Score")
cat("Slide 68: Observation ⇒ Post scores are generally higher than pre scores.\n")
t_p <- t.test(scores$pre.score, scores$post.score, paired=TRUE)$p.value
wil_p <- wilcox.test(scores$pre.score, scores$post.score, paired=TRUE)$p.value
cat(sprintf("Slide 68: Paired t-test p=%.4f; Wilcoxon p=%.4f\n", t_p, wil_p))

#================================================================================
# Slide 69: Paired Control vs Drug (drugtesting.csv)
#================================================================================
drug <- read.csv("drugtesting.csv")
boxplot(drug$Control, drug$Drug, names=c("Control","Drug"),
        main="Slide 69: Control vs Drug", ylab="Response")
cat("Slide 69: Observation ⇒ Drug treatment response is lower than control.\n")
t_p2  <- t.test(drug$Control, drug$Drug, paired=TRUE)$p.value
wil_p2 <- wilcox.test(drug$Control, drug$Drug, paired=TRUE)$p.value
cat(sprintf("Slide 69: Paired t-test p=%.4f; Wilcoxon p=%.4f\n", t_p2, wil_p2))

#================================================================================
# Slide 70: Two-Sample Woman vs Man Weights
#================================================================================
women_weight <- c(38.9,61.2,73.3,21.8,63.4,64.6,48.4,48.8,48.5)
men_weight   <- c(67.8,60,63.4,76,89.4,73.3,67.3,61.3,62.4)
my_data2 <- data.frame(group=rep(c("Woman","Man"), each=9),
                      weight=c(women_weight, men_weight))
boxplot(weight~group, data=my_data2,
        main="Slide 70: Weight by Gender", ylab="Weight")
cat("Slide 70: Observation ⇒ Men tend to have higher weights than women.\n")
cat(sprintf("Slide 70: t-test p=%.4f; Wilcoxon p=%.4f\n",
            t.test(weight~group, data=my_data2)$p.value,
            wilcox.test(weight~group, data=my_data2)$p.value))

#================================================================================
# Slide 88: Chi-Square T/F & MCQs
#================================================================================
cat("Slide 88 Answers:\n",
    "1. TRUE\n2. FALSE\n3. TRUE\n4. C\n5. B\n", sep="")

#================================================================================
# Slide 89: Chi-Square Goodness-of-Fit Die
#================================================================================
cat("Slide 89 Answers:\n",
    "6. TRUE\n",
    "7. Expected frequency per face = 120/6 = 20\n", sep="")

#================================================================================
# Slide 104: Fisher’s Exact Overview
#================================================================================
cat("Slide 104 Answer: E\n")

#================================================================================
# Slide 105: Fisher’s Exact Details
#================================================================================
cat("Slide 105 Answers:\n",
    "2. C\n3. FALSE\n", sep="")

#================================================================================
# Slide 116: Correlation Basics
#================================================================================
x116 <- c(0,3,6,7,9); y116 <- c(0,1.4,2.6,3.8,7.2)
r116 <- cor(x116, y116)
cat(sprintf("Slide 116–1: r ≈ %.3f\n", r116))
cat("Slide 116–2: C\nSlide 116–3: B\n", sep="")

#================================================================================
# Slide 117: Covariance & Correlation of ABC vs XYZ
#================================================================================
abc_ret <- c(6,8,10); xyz_ret <- c(4,5,5.5)
probs   <- c(0.15,0.60,0.25)
mu_abc  <- sum(abc_ret * probs)
mu_xyz  <- sum(xyz_ret * probs)
covar   <- sum((abc_ret-mu_abc)*(xyz_ret-mu_xyz)*probs)
corrc   <- covar / (sd(abc_ret)*sd(xyz_ret))
cat(sprintf("Slide 117: covariance = %.4f; correlation = %.4f\n", covar, corrc))

#================================================================================
# Slide 118: faithful Geyser Data
#================================================================================
plot(faithful$eruptions, faithful$waiting,
     main="Slide 118: Eruption vs Waiting", xlab="Eruption Length", ylab="Waiting Time")
abline(lm(waiting ~ eruptions, data=faithful))
cov118 <- cov(faithful$eruptions, faithful$waiting)
cor118 <- cor(faithful$eruptions, faithful$waiting)
cat(sprintf("Slide 118: covariance = %.4f; correlation = %.4f\n", cov118, cor118))
cat("Slide 118: Observation ⇒ Two clusters correspond to short vs long eruptions.\n")

#================================================================================
# Slide 119: Significance of Correlation
#================================================================================
r119 <- 0.42; n119 <- 60
t_stat <- r119 * sqrt((n119-2)/(1 - r119^2))
crit   <- qt(0.975, n119-2)
cat(sprintf("Slide 119–6: t = %.3f; critical = %.3f ⇒ %s\n", 
            t_stat, crit, ifelse(abs(t_stat) > crit, "significant", "not significant")))
cat("Slide 119–7: TRUE\n")
