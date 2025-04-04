##############################
# Name: Jacob Jarrett
# Homework #10 
##############################

#================================================================================
### === SLIDE 149 ===   
#================================================================================
# (Hypergeometric distribution examples)

# Question 1: Probability that exactly 2 red balls are drawn
prob2 <- choose(4, 2) * choose(10, 3) / choose(14, 5)
print(prob2)   # Alternatively: dhyper(2, 4, 10, 5)

# Question 2: Probability that exactly 3 red balls are drawn
prob3 <- choose(4, 3) * choose(10, 2) / choose(14, 5)
print(prob3)   # Alternatively: dhyper(3, 4, 10, 5)

# Question 3: Probability that at least 2 red balls are drawn
prob_at_least2 <- sum(dhyper(2:4, 4, 10, 5))
print(prob_at_least2)

# Question 4: Probability that zero red balls are drawn
prob0 <- choose(4, 0) * choose(10, 5) / choose(14, 5)
print(prob0)   # Alternatively: dhyper(0, 4, 10, 5)


#================================================================================
### === SLIDE 158 ===   
#================================================================================
# (Multinomial distribution examples)

# Question 1:
# Suppose a rating scale has four categories with probabilities (0.4, 0.2, 0.05, 0.01).
# Find the probability of observing 5 excellent, 2 good, 2 fair, and 1 poor.
# NOTE: In a typical multinomial, the probabilities should sum to 1.
multinom1 <- dmultinom(c(5, 2, 2, 1), prob = c(0.4, 0.2, 0.05, 0.01))
print(multinom1)

# Question 2:
# An urn has 2 red, 3 green, and 4 blue balls (total = 9).
# Find the probability (with replacement) of drawing 3 green, 1 red, and 1 blue.
# Here, the count vector must correspond to the same order as the probability vector.
multinom2 <- dmultinom(c(3, 1, 1), prob = c(3/9, 2/9, 4/9))
print(multinom2)


#================================================================================
### === SLIDE 167 ===   
#================================================================================
# (Poisson distribution examples)

# Question 1: For a Poisson variable with λ = 2, find P(X ≥ 4)
p_ge4 <- 1 - ppois(3, lambda = 2)
print(p_ge4)

# Question 2: If X is the sum of two independent Poisson variables with λ = 2.3 and 3.1,
# then the combined λ = 5.4. Find P(X = 7).
p_eq7 <- dpois(7, lambda = 5.4)
print(p_eq7)

# Question 3: If X is the sum of two independent Poisson variables with λ = 1.7 and 2.9,
# then the combined λ = 4.6. Find P(X ≥ 6).
p_ge6 <- 1 - ppois(5, lambda = 4.6)
print(p_ge6)


#================================================================================
### === PDF: Mathematical Expectations ===   
#================================================================================
# Below are the answers to the PDF questions (1–13).

# 1) Mean of a random variable X is given by _________
#    Correct answer: (a) E(X)

# 2) Variance of a random variable X is given by _________
#    Correct answer: (c) E(X^2) – (E(X))^2

# 3) Find the mean and variance of X, given the table:
#        x :  0   1   2   3   4
#      p(x): 1/9 2/9 3/9 2/9 1/9
#    E(X)  = (0*1/9) + (1*2/9) + (2*3/9) + (3*2/9) + (4*1/9) 
#           = 2
#    E(X^2) = (0^2*1/9) + (1^2*2/9) + (2^2*3/9) + (3^2*2/9) + (4^2*1/9)
#           = 16/3
#    Var(X) = E(X^2) - [E(X)]^2 = 16/3 - (2)^2 = 16/3 - 4 = 4/3 ≈ 1.3333

# 4) Find the expectation of a random variable X (multiple choice).
#    Often the distribution is x=0,1,2,3 with p=1/6,2/6,2/6,1/6.
#    E(X) = 0*(1/6) + 1*(2/6) + 2*(2/6) + 3*(1/6) = 9/6 = 1.5
#    Correct answer: (b) 1.5

# 5) E(X) = npq is for which distribution?
#    Correct answer: (b) Binomial

# 6) E(X) = λ is for which distribution?
#    Correct answer: (c) Poisson’s

# 7) E(X) = μ and V(X) = σ² is for which distribution?
#    Correct answer: (d) Normal

# 8) A random variable X takes values 3, 6, 9 (each with probability 1/3).
#    We want E[(2X+1)^3].
#    (2*3+1)^3 = 7^3   = 343
#    (2*6+1)^3 = 13^3  = 2197
#    (2*9+1)^3 = 19^3  = 6859
#    E[(2X+1)^3] = (1/3)*(343 + 2197 + 6859) = (1/3)*9399 = 3133

# 9) X has possible values {-2, -1, 0, 1, 2} with P(X=-2)=P(X=-1)=p, and
#    P(X=0)=0.4, P(X=1)=0.1, P(X=2)=0.1. Then:
#    2p + 0.4 + 0.1 + 0.1 = 1 => 2p = 0.4 => p=0.2.
#    E(X) = -2*(0.2) + -1*(0.2) + 0*(0.4) + 1*(0.1) + 2*(0.1)
#         = -0.4 -0.2 + 0 + 0.1 + 0.2 = -0.3

# 10) A discrete distribution: x = 0, 1, 2, 3, 4 each with p=1/5 (uniform).
#    a) E(X) = (0+1+2+3+4)/5 = 10/5 = 2
#    b) E(X^2) = (0^2 + 1^2 + 2^2 + 3^2 + 4^2)/5 = 30/5 = 6
#       Var(X) = 6 - 2^2 = 6 - 4 = 2
#       SD(X) = sqrt(2) ≈ 1.4142

# 11) A random variable X can take only two values, 4 and 5, with P(4)=0.32, P(5)=0.47.
#    E(X)   = 4*0.32 + 5*0.47 = 1.28 + 2.35 = 3.63
#    E(X^2) = 16*0.32 + 25*0.47 = 5.12 + 11.75 = 16.87
#    Var(X) = 16.87 - (3.63)^2 = 16.87 - 13.1769 = 3.6931 ≈ 3.7
#    Matches option (c) 3.7

# 12) The random variable x represents the number of credit cards that adults have
#    (with some probabilities). The mean and standard deviation are found via:
#    E(X) = Σ [x * p(x)], Var(X) = E(X^2) - [E(X)]^2, SD(X) = sqrt(Var(X))
#    (Use the actual table values if provided.)

# 13) 15 observations of x, with Σx=170 and Σx^2=2830. 
#    Σx  = 170 - 20 + 30 = 180
#    Σx^2= 2830 - (20^2) + (30^2) = 2830 - 400 + 900 = 3330
#    Mean = 180 / 15 = 12
#    Var(X) = (Σx^2 / n) - (Mean)^2 = (3330/15) - (12^2) = 222 - 144 = 78
#    Matches option (B) 78.0
