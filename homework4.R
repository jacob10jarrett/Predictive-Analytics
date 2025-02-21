#===========================================
# Name:  Jacob Jarrett
#
# Homework #4
#===========================================


#===========================================
# Topic 1 - Part 2 - Slide 33
# (Armstrong Number Checker)
#===========================================

num <- as.integer(readline(prompt="Enter a number: "))
len <- as.integer(readline(prompt="Enter the length of the number: "))

temp <- num
sum_of_powers <- 0

while (temp > 0) {
  digit <- temp %% 10           # get the last digit
  sum_of_powers <- sum_of_powers + digit^len
  temp <- temp %/% 10           # remove the last digit
}

if (sum_of_powers == num) {
  cat(num, "is an Armstrong number\n")
} else {
  cat(num, "is NOT an Armstrong number\n")
}


#===========================================
# Topic 1 - Part 2 - Slide 34
# (Reference: Armstrong Number Table)
#===========================================

test_numbers <- c(1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208,9474)
cat("Known Armstrong numbers:", test_numbers, "\n")


#===========================================
# Topic 1 - Part 2 - Slide 35
# (Fibonacci Sequence)
#===========================================

Fibonacci <- numeric(15)  # create a numeric vector of length 15
Fibonacci[1] <- 1
Fibonacci[2] <- 1

for(i in 3:15) {
  Fibonacci[i] <- Fibonacci[i - 1] + Fibonacci[i - 2]
}

cat("First 15 Fibonacci numbers:\n")
print(Fibonacci)


#===========================================
# Topic 1 - Part 2 - Slide 56
# (Function to Determine Number of Digits)
#===========================================

nDigits <- function(x) {
  # nchar(trunc(abs(x))) is a concise way to do it
  nchar(trunc(abs(x)))
}

num <- as.numeric(readline(prompt="Enter an integer: "))
len <- nDigits(num)
cat("The number has", len, "digits.\n")


#===========================================
# Topic 1 - Part 2 - Slide 57
# (Function "Times" - Count Occurrences in a Vector)
#===========================================


Times <- function(vec, val) {
  count <- sum(vec == val)
  if (count == 1) {
    return(paste("The number appears", count, "time in the vector."))
  } else {
    return(paste("The number appears", count, "times in the vector."))
  }
}

n <- as.integer(readline(prompt="How many numbers do you want to enter? "))
numbers <- numeric(n)

for (i in 1:n) {
  numbers[i] <- as.numeric(readline(prompt="Enter a number: "))
}

int_val <- as.numeric(readline(prompt="Enter an integer to search for: "))

result <- Times(numbers, int_val)
cat(result, "\n")



#===========================================
# Topic 1 - Part 2 - Slide 69
# (Creating Tables in R)
#===========================================

set.seed(123)  # for reproducibility
myPoisson <- rpois(1000, lambda = 10)
cat("Table of Poisson-distributed values:\n")
print(table(myPoisson))

cat("\nAverage Sepal.Length by Species:\n")
print(tapply(iris$Sepal.Length, iris$Species, mean))

cat("\nAverage Sepal.Width by Species:\n")
print(tapply(iris$Sepal.Width, iris$Species, mean))

cat("\nAverage Petal.Length by Species:\n")
print(tapply(iris$Petal.Length, iris$Species, mean))

cat("\nAverage Petal.Width by Species:\n")
print(tapply(iris$Petal.Width, iris$Species, mean))

cat("\nMean mpg by cyl, vs, gear:\n")
print(with(mtcars, tapply(mpg, list(cyl, vs, gear), mean)))

cat("\nMean mpg by cyl, vs, gear, carb in a flat table:\n")
# One approach is to use aggregate, then reshape:
library(reshape2)  # if not installed, install.packages("reshape2")
agg_data <- aggregate(mpg ~ cyl + vs + gear + carb, data = mtcars, FUN = mean)
# Print the aggregated data in "long" form:
print(agg_data)

# Reshape to a wide "flat" format if desired:
flat_table <- dcast(agg_data, cyl + vs + gear ~ carb, value.var = "mpg")
print(flat_table)


