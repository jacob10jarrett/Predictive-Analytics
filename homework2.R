#===========================================
# Name:  Jacob Jarrett 
#
# Assignment 2 
#===========================================


#===========================================
# Topic 1 - Part 1 - Slide #66
#===========================================


A <- -2.7
B <- -0.5
C <- 0.3
D <- 1.5
E <- 2.8

functions <- data.frame(
  Function = c("floor", "ceiling", "trunc", "round"),
  A = c(floor(A), ceiling(A), trunc(A), round(A)),
  B = c(floor(B), ceiling(B), trunc(B), round(B)),
  C = c(floor(C), ceiling(C), trunc(C), round(C)),
  D = c(floor(D), ceiling(D), trunc(D), round(D)),
  E = c(floor(E), ceiling(E), trunc(E), round(E))
)

functions


#===========================================
# Topic 1 - Part 1 - Slide #67
#===========================================

install.packages("readxl")
install.packages("data.table")
library(readxl)
library(data.table)
data(package = "readxl")
data(package = "data.table")

data <- read.csv("http://apps.fs.fed.us/fiad-bdownloads/CSV/LICHEN_SPECIES_SUMMARY.csv")
data2 <- read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/cancer.txt", header = TRUE)
data3 <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/mtcars.csv")

head(data)
tail(data)
str(data)

head(data2)
tail(data2)
str(data2)

head(data3)
tail(data3)
str(data3)


#===========================================
# Topic 1 - Part 1 - Slide #68
#===========================================

education <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/robustbase/education.csv", stringsAsFactors = FALSE)
colnames(education) <- c("X","State","Region","Urban.Population","Per.Capita.Income","Minor.Population","Education.Expenditures")

Urban.Population <- c(mean(education$Urban.Population), sd(education$Urban.Population), range(education$Urban.Population))
Per.Capita.Income <- c(mean(education$Per.Capita.Income), sd(education$Per.Capita.Income), range(education$Per.Capita.Income))
Minor.Population <- c(mean(education$Minor.Population), sd(education$Minor.Population), range(education$Minor.Population))
Education.Expenditures <- c(mean(education$Education.Expenditures), sd(education$Education.Expenditures), range(education$Education.Expenditures))

Urban.Population
Per.Capita.Income
Minor.Population
Education.Expenditures


#===========================================
# Topic 1 - Part 1 - Slide #84
#===========================================

install.packages("nycflights13")
install.packages("dplyr")

library(nycflights13)
library(dplyr)

# 1. Arrival delay of two or more hours
flights_1 <- filter(flights, arr_delay >= 120)

# 2. Flew to Houston (IAH or HOU)
flights_2 <- filter(flights, dest %in% c("IAH", "HOU"))

# 3. Operated by United, American, or Delta
flights_3 <- filter(flights, carrier %in% c("UA", "AA", "DL"))

# 4. Arrived more than two hours late, but didn’t leave late
flights_4 <- filter(flights, arr_delay > 120, dep_delay <= 0)

# 5. Delayed by at least an hour, but made up over 30 minutes in flight
flights_5 <- filter(flights, dep_delay >= 60, (dep_delay - arr_delay) > 30)

# 6. Departed between midnight and 6am (inclusive)
flights_6 <- filter(flights, dep_time >= 0, dep_time <= 600)

# 7. How many flights have a missing dep_time?
missing_dep_time_count <- sum(is.na(flights$dep_time))
missing_dep_time_count


#===========================================
# Topic 1 - Part 1 - Slide #85
#===========================================


A <- matrix(c(8, 1, 4, 5, 6, 3, 9, 10, 12, 23, 44, 32), byrow = TRUE, nrow = 3)
A

A[1,1]
A[,4]
A[c(2,3), c(1,3)]
A[, 2:4]

matrix_a <- matrix(1:10, byrow = TRUE, nrow = 5)
matrix_a


#===========================================
# Topic 1 - Part 1 - Slide #86
#===========================================


matrix_b <- matrix(1:10, byrow = FALSE, nrow = 5)
matrix_b

x <- matrix(c(50, 37, 72, 87, 78, 45), ncol = 2)
x

matrix_d <- matrix(1:12, byrow = FALSE, ncol = 3)
matrix_d


#===========================================
# Topic 1 - Part 1 - Slide #87
#===========================================

matrix_a <- matrix(1:10, byrow = TRUE, nrow = 5)
matrix_b <- matrix(1:10, byrow = FALSE, nrow = 5)

matrix_c <- matrix(1:12, byrow = FALSE, ncol = 3)
add_row <- c(1, 2, 3) 
matrix_c <- rbind(matrix_c, add_row)

dim(matrix_c)

matrix_c[1, 2]  
matrix_c[1:3, 2:3]  


#===========================================
# Topic 1 - Part 1 - Slide #88
#===========================================

Correlation Pair Rank
Height & Volume: 2
Girth & Volume: 1
Girth & Height: 3