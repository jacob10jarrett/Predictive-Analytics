#=====================================================
# Homwork 5
# Name: Jacob Jarrett
#=====================================================

# --- Slide 22: Load and Inspect the Chicago Dataset ---
chicago <- read.csv("chicago.csv", stringsAsFactors = FALSE)

if("date" %in% names(chicago)){
  chicago$date <- as.Date(chicago$date)
}

head(chicago)
str(chicago)
summary(chicago)


# --- Slide 26: Summary Statistics for Key Variables ---
if("temp" %in% names(chicago)) {
  cat("Temperature Summary:\n")
  print(summary(chicago$temp))
} else {
  cat("Column 'temp' not found.\n")
}

if("wind" %in% names(chicago)) {
  cat("\nWind Speed Summary:\n")
  print(summary(chicago$wind))
} else {
  cat("Column 'wind' not found.\n")
}

if("pm25" %in% names(chicago)) {
  cat("\nPM2.5 Summary:\n")
  print(summary(chicago$pm25))
} else {
  cat("Column 'pm25' not found.\n")
}


# --- Slide 29: Histogram of Temperature ---
if("temp" %in% names(chicago)) {
  hist(chicago$temp,
       main = "Histogram of Temperature",
       xlab = "Temperature",
       col = "lightblue",
       border = "black")
} else {
  cat("Cannot create histogram: 'temp' column not found.\n")
}


# --- Slide 42: Subset Data for Above-Average Temperature Days ---
if("temp" %in% names(chicago)) {
  avg_temp <- mean(chicago$temp, na.rm = TRUE)
  chicago_high_temp <- subset(chicago, temp > avg_temp)
  cat("\nNumber of days with above-average temperature:", nrow(chicago_high_temp), "\n")
} else {
  cat("Cannot subset data: 'temp' column not found.\n")
}


# --- Slide 47: Scatterplot of Temperature vs. Wind Speed ---
if(all(c("temp", "wind") %in% names(chicago))) {
  plot(chicago$temp, chicago$wind,
       main = "Scatterplot: Temperature vs. Wind Speed",
       xlab = "Temperature",
       ylab = "Wind Speed",
       pch = 19, 
       col = "darkgreen")
} else {
  cat("Cannot create scatterplot: 'temp' and/or 'wind' column not found.\n")
}


# --- Slide 50: Linear Regression: Predict Wind Speed from Temperature ---
if(all(c("temp", "wind") %in% names(chicago))) {
  model <- lm(wind ~ temp, data = chicago)
  cat("\nLinear Regression Summary:\n")
  print(summary(model))
  
  # Re-plot the scatterplot and add the regression line
  plot(chicago$temp, chicago$wind,
       main = "Regression: Wind Speed vs. Temperature",
       xlab = "Temperature",
       ylab = "Wind Speed",
       pch = 19, 
       col = "darkgreen")
  abline(model, col = "red", lwd = 2)
} else {
  cat("Cannot perform linear regression: 'temp' and/or 'wind' column not found.\n")
}
