##############################
# Name: Jacob Jarrett
# Homework 7
##############################

#================================================================================
### === SLIDE 12: Pollution Map for North Carolina === 
#================================================================================

# Load required packages
library(maps)
library(dplyr)

# Read in the dataset (ensure the CSV file "avgpm25-1.csv" is in your working directory)
pollution <- read.csv("avgpm25-1.csv", stringsAsFactors = FALSE)

# Convert the fips column to character class
pollution$fips <- as.character(pollution$fips)
print(class(pollution$fips))

# One Dimension Summaries for PM2.5 values
print(fivenum(pollution$pm25))
print(summary(pollution$pm25))
boxplot(pollution$pm25, col = "blue", main = "Boxplot of PM2.5 Levels")

# Filter the data for North Carolina
state_pollution <- filter(pollution, State.Name == "North Carolina")

# Optionally, create a new variable with rounded PM2.5 values (for color-coding)
state_pollution$pm25_rounded <- round(state_pollution$pm25)

# Create the base map for North Carolina counties
map("county", "north carolina", fill = TRUE, col = "lightgray", bg = "white", resolution = 0)

# Plot points for observations with PM2.5 levels above 15 (optional)
with(filter(state_pollution, pm25 > 15),
     points(longitude, latitude, pch = 16, col = "red"))

# Alternatively, plot points color-coded by the rounded PM2.5 values
with(state_pollution,
     points(longitude, latitude, pch = 16, col = pm25_rounded))

# Add a legend that does not overlap the map.
legend("bottomleft", 
       legend = sort(unique(state_pollution$pm25_rounded)),
       col = sort(unique(state_pollution$pm25_rounded)), 
       pch = 16, 
       title = "PM2.5 Levels", 
       bty = "n")
