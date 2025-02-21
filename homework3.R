#======================================================
# Name: Jacob Jarrett
#
# Homework 3
#======================================================

#------------------------------------------------------
# Topic 1 - Part 1 - Slide 1: mtcars Plotting
#------------------------------------------------------

# (a) Assign the mtcars dataset to 'mydata'
mydata <- mtcars

# (b) Inspect the dataset
cat("\n--- mtcars: Head, Tail, and Summary ---\n")
head(mydata)
tail(mydata)
summary(mydata)

# (c) Create a histogram of 'mpg'
hist(mydata$mpg,
     main = "Histogram of MPG",
     xlab = "Miles per Gallon",
     col  = "lightblue"
)

# (d) Create a histogram of 'mpg' with more breaks
hist(mydata$mpg,
     breaks = 15,
     main = "Histogram of MPG with More Breaks",
     xlab = "Miles per Gallon",
     col  = "lightgreen"
)

# (e) Create a boxplot of 'mpg' for each number of cylinders
boxplot(mydata$mpg ~ mydata$cyl,
        main = "Boxplot of MPG by Number of Cylinders",
        xlab = "Number of Cylinders",
        ylab = "Miles per Gallon",
        col  = "lightyellow"
)

# (f) Create a scatterplot of 'mpg' vs. 'cyl'
plot(mydata$cyl, mydata$mpg,
     main = "Scatterplot of MPG vs. Cylinders",
     xlab = "Number of Cylinders",
     ylab = "Miles per Gallon",
     pch  = 19,
     col  = "blue"
)

# (g) Plot horsepower vs mpg and add a regression line
plot(mydata$mpg, mydata$hp,
     main = "Scatterplot of Horsepower vs. MPG",
     xlab = "Miles per Gallon",
     ylab = "Horsepower",
     pch  = 19,
     col  = "red"
)
hp_model <- lm(hp ~ mpg, data = mydata)
abline(hp_model, col = "darkgreen", lwd = 2)

#------------------------------------------------------
# Topic 1 - Part 1 - Slide 2: iris Data Manipulation
#------------------------------------------------------

cat("\n--- iris: Head, Tail, and Summary ---\n")
head(iris)
tail(iris)
summary(iris)

# (b) Filter for Species 'versicolor' and store in iris.vers
iris.vers <- subset(iris, Species == "versicolor")

# (c) Create 'sepal.dif' (Sepal.Length - Sepal.Width)
sepal.dif <- iris.vers$Sepal.Length - iris.vers$Sepal.Width

# (d) Add new column 'sepal.dif' to 'iris.vers'
iris.vers$sepal.dif <- sepal.dif

# (e) Filter for 'virginica' with Sepal.Width > 3.5
iris.filtered <- subset(iris, Species == "virginica" & Sepal.Width > 3.5)

# (f) Rows where integer part of Sepal.Length is odd
iris.odd <- subset(iris, as.integer(Sepal.Length) %% 2 == 1)

# (g) Calculate the mean of each numeric variable in iris
numeric_cols <- sapply(iris, is.numeric)
mean_values  <- sapply(iris[, numeric_cols], mean)
cat("\n--- Means of Numeric Variables in iris ---\n")
print(mean_values)

#------------------------------------------------------
# Topic 1 - Part 1 - Slide 3: Nested if...else (Age)
#------------------------------------------------------

# 1. Prompt the user to enter their age and ensure it's numeric
my.age <- as.integer(readline(prompt = "\nPlease Enter your Age: "))

# 2. Loop until a valid numeric age is entered
while (is.na(my.age)) {
  cat("Invalid input. Please enter a valid numeric age.\n")
  my.age <- as.integer(readline(prompt = "Please Enter your Age: "))
}

# 3. Evaluate input using nested if...else
if (my.age < 18) {
  cat("You are Not a Major.\n")
  cat("You are Not Eligible to Work.\n")
} else if (my.age >= 18 && my.age <= 60) {
  cat("You are Eligible to Work.\n")
  cat("Please fill the Application Form and Email to us.\n")
} else if (my.age > 60) {
  cat("As per the Government Rules, You are too Old to Work.\n")
  cat("Please collect your pension!\n")
}

cat("This Message is from Outside the Nested IF Else Statement\n")

#------------------------------------------------------
# Topic 1 - Part 1 - Slide 4: if else() Apple Stocks
#------------------------------------------------------

# 1. Apple stock prices
apple <- c(109.49, 109.90, 109.11, 109.95, 111.03, 112.12)

# 2. ifelse() to decide if to buy each stock (<110 => buy)
decision <- ifelse(apple < 110, "buy the apple stock", "don't buy the apple stock")

cat("\n--- Apple Stock Decisions ---\n")
print(decision)

cat("\nAll code executed successfully!\n")

