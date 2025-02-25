##############################
# Name: Jacob Jarrett 
# Homework 5
##############################

### === SLIDE 22: Chicago Dataset Analysis ===

# 1. Load the chicago_data into variable "Chicago" (capital “C” as required)
Chicago <- read.csv("chicago.csv", stringsAsFactors = FALSE)
if("date" %in% names(Chicago)) {
  Chicago$date <- as.Date(Chicago$date)
}

# 2. Examine the completeness of key variables with histograms.
# Required variables: tmpd, dptp, pm25tmeans2, pm10tmeans2, o3means2, no2mean2.
vars_to_plot <- c("tmpd", "dptp", "pm25tmeans2", "pm10tmeans2", "o3means2", "no2mean2")
for (var in vars_to_plot) {
  if (var %in% names(Chicago)) {
    hist(Chicago[[var]], main = paste("Histogram of", var),
         xlab = var, col = "lightblue", border = "black")
  } else {
    cat("Column", var, "not found in Chicago dataset.\n")
  }
}

# 3. Answer: How might missing data bias the analysis?
# ------------------------------------------------------------------------------
# Missing data can lead to biased estimates if the data are not missing completely at random.
# For example, if measurements (like extreme temperatures or pollutant levels) are missing more often
# under certain conditions, the summary statistics and any models built on the data may underestimate
# variability or misrepresent the true averages and relationships.
# In our analyses, if missing data occur more frequently during extreme weather events, we might
# underestimate the severity or frequency of those events.
# ------------------------------------------------------------------------------

# 4. Seasonality pattern in o3 and no2 by temperature deciles.
library(dplyr)
if(all(c("tmpd", "o3means2", "no2mean2") %in% names(Chicago))){
  Chicago <- Chicago %>% mutate(temp_decile = ntile(tmpd, 10))
  seasonality_temp <- Chicago %>% 
    group_by(temp_decile) %>% 
    summarize(avg_o3  = mean(o3means2, na.rm = TRUE),
              avg_no2 = mean(no2mean2, na.rm = TRUE))
  print(seasonality_temp)
} else {
  cat("Required columns for temperature decile analysis not found.\n")
}

# 5. Seasonality pattern in o3 and no2 by dew point deciles.
if(all(c("dptp", "o3means2", "no2mean2") %in% names(Chicago))){
  Chicago <- Chicago %>% mutate(dewpoint_decile = ntile(dptp, 10))
  seasonality_dew <- Chicago %>% 
    group_by(dewpoint_decile) %>% 
    summarize(avg_o3  = mean(o3means2, na.rm = TRUE),
              avg_no2 = mean(no2mean2, na.rm = TRUE))
  print(seasonality_dew)
} else {
  cat("Required columns for dew point decile analysis not found.\n")
}

# 6. Conclusions from the analyses:
# ------------------------------------------------------------------------------
# Based on the decile analyses, one might observe trends such as:
# - A possible increase in average ozone (o3means2) with higher temperature deciles,
#   though this relationship may not be strictly linear.
# - Nitrogen dioxide (no2mean2) may show a different pattern that could be influenced by atmospheric conditions.
# - When using dew point deciles, similar patterns may emerge, highlighting how moisture-related variables
#   interact with pollutant levels.
# ------------------------------------------------------------------------------

### === SLIDES 25 & 26: EPA Ozone Dataset Analysis ===

library(data.table)
# Load the EPA hourly ozone dataset 
ozone <- fread("hourly_44201_2014.csv")

# Basic exploration of the dataset:
str(ozone)
head(ozone)
tail(ozone)
names(ozone)

# Rename columns as specified (if available)
if(ncol(ozone) >= 23) {
  names(ozone)[22] <- "StateName"
  names(ozone)[14] <- "SampleMeasurement"
  names(ozone)[23] <- "CountyName"
} else {
  cat("Not enough columns to rename in ozone dataset.\n")
}

# Further exploration:
table(ozone$`Time Local`)
cat("Unique States in ozone dataset:", nrow(unique(select(ozone, StateName))), "\n")
quantile(ozone$SampleMeasurement, seq(0, 1, 0.1), na.rm = TRUE)

# Ranking by State and County based on average ozone
ranking <- ozone %>%
  group_by(StateName, CountyName) %>%
  summarize(ozone_avg = mean(SampleMeasurement, na.rm = TRUE), n = n()) %>%
  as.data.frame() %>%
  arrange(desc(ozone_avg))
head(ranking)

# ------------------------------------------------------------------------------
# Answer: Yes, the code successfully loads and processes the dataset. 
# ------------------------------------------------------------------------------

### === SLIDE 42: Exploratory Data Analysis using ggplot2 ===

library(ggplot2)

# 1. Relationship between cty (city) and hwy (highway) in the mpg dataset.
p1 <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  ggtitle("City vs. Highway Mileage")
print(p1)
# ------------------------------------------------------------------------------
# Answer: There is a positive relationship between city (cty) and highway (hwy) mileage,
# indicating that vehicles which perform well in the city tend to also perform well on highways.
# ------------------------------------------------------------------------------

# 2. Plot of model vs. manufacturer in the mpg dataset.
p2 <- ggplot(mpg, aes(x = model, y = manufacturer)) +
  geom_point() +
  ggtitle("Model vs. Manufacturer")
print(p2)
# ------------------------------------------------------------------------------
# Answer: This plot shows the relationship between car models and their manufacturers.
# It can be hard to interpret because of overplotting; many points overlap, making it less informative.
# A better approach might be to jitter the points or to summarize the counts of observations per combination.
# ------------------------------------------------------------------------------

# 3. Additional plots:
# a) mpg: cty vs. hwy scatterplot (again for clarity)
p3 <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  ggtitle("mpg: City vs. Highway Mileage")
print(p3)
# Expected: A positive trend with clusters of points showing similar fuel efficiency ratings.

# b) diamonds: carat vs. price scatterplot
p4 <- ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.3) +
  ggtitle("Diamonds: Carat vs. Price")
print(p4)
# Expected: A strong positive correlation where price increases rapidly with carat size,
# likely in a non-linear fashion.

# c) economics: date vs. unemploy line plot
p5 <- ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line() +
  ggtitle("Economics: Unemployment Over Time")
print(p5)
# Expected: A line plot that captures trends and cycles in unemployment over time.

# d) mpg: histogram of city mileage (cty)
p6 <- ggplot(mpg, aes(x = cty)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  ggtitle("Histogram of City Mileage (mpg)")
print(p6)
# Expected: A unimodal (or possibly skewed) distribution showing the frequency of different city mileage values.

### === SLIDE 47: Drive Train and Fuel Economy Analysis ===

# Relationship between drive train and highway fuel economy.
p7 <- ggplot(mpg, aes(x = drv, y = hwy)) +
  geom_boxplot() +
  ggtitle("Highway Fuel Economy by Drive Train")
print(p7)
# ------------------------------------------------------------------------------
# Answer: The boxplot shows that different drive train types (e.g., front-wheel, rear-wheel, 4-wheel)
# exhibit different highway fuel economies. Typically, front-wheel drive vehicles tend to have better fuel economy.
# ------------------------------------------------------------------------------

# Relationship between drive train, engine size (displ), and highway mileage.
p8 <- ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  ggtitle("Engine Displacement vs. Highway Mileage by Drive Train")
print(p8)
# ------------------------------------------------------------------------------
# Answer: This scatterplot indicates that as engine displacement increases, highway mileage generally decreases.
# The coloring by drive train helps reveal that the relationship may vary by drivetrain type.
# ------------------------------------------------------------------------------

# Relationship between vehicle class and highway mileage.
p9 <- ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot() +
  ggtitle("Highway Mileage by Vehicle Class")
print(p9)
# ------------------------------------------------------------------------------
# Answer: This boxplot shows the variation in highway mileage across different vehicle classes.
# Some classes (e.g., compact cars) typically have higher mileage compared to SUVs or trucks.
# ------------------------------------------------------------------------------

### === SLIDE 50: Facetting in ggplot2 ===

# 1. Facet by a continuous variable.
p10 <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  facet_wrap(~hwy) +
  ggtitle("Faceting by Continuous Variable 'hwy'")
print(p10)
# ------------------------------------------------------------------------------
# Answer: Faceting by a continuous variable such as hwy creates too many panels, making it cluttered
# and difficult to interpret. This demonstrates why continuous variables are usually not ideal for faceting.
# ------------------------------------------------------------------------------

# Facet by a discrete variable like cyl (number of cylinders).
p11 <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  facet_wrap(~cyl) +
  ggtitle("Faceting by Number of Cylinders ('cyl')")
print(p11)
# ------------------------------------------------------------------------------
# Answer: Faceting by cyl, a categorical variable, creates a small number of panels (one per cylinder group),
# making it easier to compare the relationships between city and highway mileage across these groups.
# ------------------------------------------------------------------------------

# 2. Three-way relationship: fuel economy, engine size, and number of cylinders.
p12 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~cyl) +
  ggtitle("Displacement vs. Highway Mileage Faceted by Cylinders")
print(p12)
# ------------------------------------------------------------------------------
# Answer: Faceting by the number of cylinders reveals that the relationship between engine displacement and
# highway mileage differs by engine configuration. This three-way analysis shows that while larger engines
# generally have lower fuel economy, the effect size and pattern vary with cylinder count.
# ------------------------------------------------------------------------------

# 3. Using facet_wrap() arguments to control layout.
p13 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~cyl, nrow = 2, ncol = 2) +
  ggtitle("Faceting with Specified Rows and Columns")
print(p13)
# ------------------------------------------------------------------------------
# Answer: The nrow and ncol arguments in facet_wrap() allow you to specify the number of rows and columns
# in the facet grid, thus controlling the layout of the plots.
# ------------------------------------------------------------------------------

# 4. The scales argument in facet_wrap():
p14 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~cyl, scales = "free") +
  ggtitle("Faceting by Cylinders with Free Scales")
print(p14)
# ------------------------------------------------------------------------------
# Answer: The scales argument in facet_wrap() controls whether all facets share the same axis scales ("fixed")
# or have their own ("free"). Using free scales can be useful when the ranges of data differ substantially between facets,
# ensuring that each plot is optimally scaled.
# ------------------------------------------------------------------------------