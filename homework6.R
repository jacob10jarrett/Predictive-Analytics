##############################
# Name: Jacob Jarrett
# Homework 6
##############################

#================================================================================
### === SLIDE 76: AutoClaims.csv Time Series Analysis ===
#================================================================================

# Load required libraries
library(ggplot2)
library(dplyr)

# 1) Load the dataset
AutoClaims <- read.csv("AutoClaims.csv", stringsAsFactors = FALSE)

# 2) Convert policy_bind_date to Date format if it exists, then filter for Q1 2015
if ("policy_bind_date" %in% names(AutoClaims)) {
  # Convert policy_bind_date to a proper Date
  AutoClaims$policy_bind_date <- as.Date(AutoClaims$policy_bind_date, format = "%m/%d/%Y")
  
  # Filter to include only records from January–March (Q1) of 2015
  AutoClaims <- AutoClaims %>%
    filter(
      !is.na(policy_bind_date),
      format(policy_bind_date, "%Y") == "2015",
      format(policy_bind_date, "%m") %in% c("01", "02", "03")
    )
  
  # Extract the month as a factor with descriptive labels
  AutoClaims$Month <- format(AutoClaims$policy_bind_date, "%m")
  AutoClaims$Month <- factor(AutoClaims$Month, 
                             levels = c("01", "02", "03"),
                             labels = c("January", "February", "March"))
}

# 3) Create a time series plot (path plot) with points and different colors for each month
#    Analyze total_claim_amount as a function of policy_annual_premium
if (all(c("policy_annual_premium", "total_claim_amount", "Month") %in% names(AutoClaims))) {
  ggplot(AutoClaims, aes(x = policy_annual_premium,
                         y = total_claim_amount,
                         color = Month,
                         group = Month)) +
    geom_line() +          
    geom_point(size = 2) + 
    ggtitle("Total Claim Amount vs. Policy Annual Premium (Q1 2015)") +
    xlab("Policy Annual Premium") +
    ylab("Total Claim Amount") +
    theme_minimal()
} else {
  cat("Columns for the requested plot not found in AutoClaims dataset.\n")
}

#================================================================================
### === SLIDE 85: Diamonds Dataset (Basic Exploration) ===
#================================================================================

# The diamonds dataset comes pre-loaded with ggplot2
data("diamonds")

# 1) Display the first 6 rows of the dataset
head(diamonds, 6)

# 2) Display the last 6 rows of the dataset
tail(diamonds, 6)

# 3) Display the structure of the dataset
str(diamonds)

# 4) Display the dimension of the dataset
dim(diamonds)

# 5) Plot a basic histogram of the prices of the dataset with binwidth of 500
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  ggtitle("Diamond Prices (binwidth = 500)") +
  xlab("Diamond Price US") +
  ylab("Frequency") +
  theme_minimal()


#================================================================================
### === SLIDE 86: Diamonds Dataset (Refined Histogram) ===
#================================================================================

# 8) Plot a basic histogram of the prices with:
#    a) binwidth of 100
#    b) title: "Diamond Price Distribution"
#    c) X axis label: "Diamond Price US"
#    d) Y axis label: "Frequency"
#    e) Facet based on cut

ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 100, fill = "lightgreen", color = "black") +
  ggtitle("Diamond Price Distribution") +
  xlab("Diamond Price US") +
  ylab("Frequency") +
  facet_wrap(~ cut) +
  theme_minimal()


#================================================================================
### === SLIDE 87: Diamonds Dataset (Summary Calculations) ===
#================================================================================

# 9) Calculate the following:
#    a) Mean of prices
mean_price <- mean(diamonds$price)
mean_price

#    b) Median of prices
median_price <- median(diamonds$price)
median_price

#    c) Maximum price
max_price <- max(diamonds$price)
max_price

#    d) Minimum price
min_price <- min(diamonds$price)
min_price

#    e) Count where cut = 'Fair'
count_fair <- sum(diamonds$cut == "Fair")
count_fair

#    f) Count where cut = 'Good'
count_good <- sum(diamonds$cut == "Good")
count_good

#    g) Count where cut = 'Very Good'
count_very_good <- sum(diamonds$cut == "Very Good")
count_very_good

#    h) Count where cut = 'Ideal'
count_ideal <- sum(diamonds$cut == "Ideal")
count_ideal

#    i) Contingency Table for cut
table(diamonds$cut)


#================================================================================
### === SLIDE 88: Diamonds Dataset (Filtering & Boxplot) ===
#================================================================================

# 10) Answer the following questions:
#    a) How many cost <= $500?
count_le_500 <- sum(diamonds$price <= 500)
count_le_500

#    b) How many cost <= $250?
count_le_250 <- sum(diamonds$price <= 250)
count_le_250

#    c) How many cost at least $15,000?
count_ge_15000 <- sum(diamonds$price >= 15000)
count_ge_15000

# 11) Display the records for the highest priced diamond
highest_price <- max(diamonds$price)
diamonds_highest <- subset(diamonds, price == highest_price)
diamonds_highest

# 12) Display the records for the lowest priced diamonds
lowest_price <- min(diamonds$price)
diamonds_lowest <- subset(diamonds, price == lowest_price)
diamonds_lowest

# 13) Display records for which cut = 'Fair'
diamonds_fair <- subset(diamonds, cut == "Fair")
head(diamonds_fair)  # Display first few for brevity

# 14) Calculate the median prices for each cut of diamonds
median_by_cut <- diamonds %>%
  group_by(cut) %>%
  summarize(median_price = median(price))
median_by_cut

# 15) Create a boxplot of diamond prices based on cut
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot(fill = "plum", color = "black") +
  ggtitle("Boxplot of Diamond Prices by Cut") +
  xlab("Cut") +
  ylab("Price") +
  theme_minimal()


#================================================================================
### === SLIDE 89: Diamonds Dataset (IQR & Middle 50% by Color) ===
#================================================================================

# 16) Price range for the middle 50% of diamonds with color D (best color).
diamonds_colorD <- subset(diamonds, color == "D")
Q1_D <- quantile(diamonds_colorD$price, 0.25)
Q3_D <- quantile(diamonds_colorD$price, 0.75)
cat("Middle 50% range for color D:", Q1_D, "to", Q3_D, "\n")

# 17) Price range for the middle 50% of diamonds with color J (worst color).
diamonds_colorJ <- subset(diamonds, color == "J")
Q1_J <- quantile(diamonds_colorJ$price, 0.25)
Q3_J <- quantile(diamonds_colorJ$price, 0.75)
cat("Middle 50% range for color J:", Q1_J, "to", Q3_J, "\n")

# 18) IQR for diamonds with the best color (color D).
IQR_D <- IQR(diamonds_colorD$price)
IQR_D

# 19) IQR for diamonds with the worst color (color J).
IQR_J <- IQR(diamonds_colorJ$price)
IQR_J

