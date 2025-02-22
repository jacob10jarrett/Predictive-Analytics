#===========================================
# Name: Jacob Jarrett 
# Homework 1
#===========================================

#===========================================
# Slide 16 - Problems 4 and 5
#===========================================

# Problem 4
a <- c(1, 2, 3, 4)
sqrt_a <- sqrt(a)
exp_a <- exp(a)
log_a <- log(a)
min_a <- min(a)
max_a <- max(a)

# Display results for Problem 4
sqrt_a
exp_a
log_a
min_a
max_a

# Problem 5
b <- c(-1, -2, -3, 4)
product_ab <- a * b
difference_ab <- a - b
division_ba <- b / a
division_ab <- a / b

# Display results for Problem 5
product_ab
difference_ab
division_ba
division_ab

#===========================================
# Slide 25 - Examples 2, 4, and 5
#===========================================

# Example 2
x <- "r tutorial"
y <- sub("r ", "HTML ", x)
y

# Example 4
str_vec <- c("Regular", "expression", "examples of R language")
x_grep <- grep("ex", str_vec, value = FALSE)
x_grep

# Example 5
split_abc <- strsplit("abc", "")
split_abc

c_str <- "14456-93234472-000087-gt44897-gh804409"
split_c <- strsplit(c_str, "-", fixed = TRUE)
split_c

# Additional String Operations
paste_x1_3 <- paste("x", 1:3, sep = "")
paste_x1_3

paste_xM1_3 <- paste("x", 1:3, sep = "M")
paste_xM1_3

paste_today <- paste("Today is", date())
paste_today

toupper_result <- toupper("abc")
toupper_result

tolower_result <- tolower("ABC")
tolower_result

#===========================================
# Slide 30 - Examples 2, 3, 4, 5, and 6
#===========================================

# Example 2
in_rep <- 1:2 %in% rep(1:2, 5)
in_rep

# Example 3
in_a <- "a" %in% c("a", "b", "c")
in_a

in_d <- "d" %in% c("a", "b", "c")
in_d

# Example 4
x_eq <- c('a', 'b', 'c')
y_eq <- c('c', 'b', 'a')
comparison_eq <- x_eq == y_eq
comparison_eq

# Example 5
comp1 <- c(1, 2, 3) == c(1, 3, 2)
comp1

comp2 <- c(5, 7, 9) == c(2, 7, 9)
comp2

# Example 6
v1 <- 3
v2 <- 101
t_vec <- c(1, 2, 3, 4, 5, 6, 7, 8)
v1_in_t <- v1 %in% t_vec
v2_in_t <- v2 %in% t_vec

# Display results for Example 6
v1_in_t
v2_in_t

# Additional Operations
x_values <- c(1, 5, 8, 4, 6)
which_eq_5 <- which(x_values == 5)
which_neq_5 <- which(x_values != 5)

# Display results
which_eq_5
which_neq_5

#===========================================
# Slide 39 - Problems 1 and 2
#===========================================

# Problem 1
sequence <- seq(10, 50, 5)
sequence

# Problem 2
sqrt_sequence <- sqrt(sequence)
log_sequence <- log(sequence)
log10_sequence <- log10(sequence)
log2_sequence <- log2(sequence)

# Display results for Problem 2
sqrt_sequence
log_sequence
log10_sequence
log2_sequence

