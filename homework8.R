##############################
# Name: Jacob Jarrett
# Homework 8 
##############################

# Load required libraries
library(ggplot2)
library(dplyr)
library(corrplot)

#================================================================================
### === SLIDE 71 ===  
#================================================================================
# Load Chicago dataset
df_chicago <- read.csv("chicago.csv")

# Display first few rows
head(df_chicago)

# Check structure and missing values
str(df_chicago)
summary(df_chicago)
sum(is.na(df_chicago))  # Count missing values

#================================================================================
### === SLIDE 72 ===  
#================================================================================
# Get descriptive statistics
summary(df_chicago)

# Additional check for missing values
any(is.na(df_chicago))

#================================================================================
### === SLIDE 73 ===  
#================================================================================
# Plot histograms for all numeric variables
par(mfrow=c(2,2))  # Arrange plots in 2x2 layout
for (col in colnames(df_chicago)) {
  if (is.numeric(df_chicago[[col]])) {
    hist(df_chicago[[col]], main=col, col="skyblue", border="black")
  }
}
par(mfrow=c(1,1))  # Reset layout

# Alternative using ggplot2 for a single variable (Replace `variable_name` accordingly)
ggplot(df_chicago, aes(x=O3tmean2)) + 
  geom_histogram(bins=20, fill="blue", color="black") +
  theme_minimal()

#================================================================================
### === SLIDE 102 ===  
#================================================================================
# Load AutoClaims dataset
df_auto <- read.csv("AutoClaims.csv")

# Display dataset dimensions and column names
dim(df_auto)
colnames(df_auto)

# Check missing values
sum(is.na(df_auto))

#================================================================================
### === SLIDE 103 ===  
#================================================================================
# Filter claims where ClaimAmount is above the mean
filtered_auto <- subset(df_auto, ClaimAmount > mean(ClaimAmount, na.rm=TRUE))

# Display first few rows of filtered data
head(filtered_auto)

# Display number of rows after filtering
nrow(filtered_auto)

#================================================================================
### === SLIDE 104 ===  
#================================================================================
# Compute correlation matrix
cor_matrix <- cor(df_auto, use="complete.obs")

# Visualize correlation matrix with better aesthetics
corrplot(cor_matrix, method="color", type="upper", tl.cex=0.8)

#================================================================================
### === SLIDE 30 ===  
#================================================================================
# Load Actor Ages dataset
df_actor <- read.csv("actor_ages.csv")

# Display first few rows
head(df_actor)

# Check structure and summary
str(df_actor)
summary(df_actor)

#================================================================================
### === SLIDE 31 ===  
#================================================================================
# Compute mean and median age
mean(df_actor$Age, na.rm=TRUE)
median(df_actor$Age, na.rm=TRUE)

# Check unique age counts
table(df_actor$Age)

#================================================================================
### === SLIDE 32 ===  
#================================================================================
# Create a bar chart of age distribution using base R
barplot(table(df_actor$Age), col="blue", main="Age Distribution", xlab="Age", ylab="Count")

# Improved version using ggplot2
ggplot(df_actor, aes(x=Age)) +
  geom_bar(fill="blue", color="black") +
  labs(title="Age Distribution", x="Age", y="Count") +
  theme_minimal()
