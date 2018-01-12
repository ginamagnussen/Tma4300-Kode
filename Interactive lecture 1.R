#TMA4300 Interactive lecture 1
# Week 2, 12.01.2018

library(MASS) # for truehist()

#Oppg A: Probability integral transform
# Note: alpha = lambda, beta = 1 is exp(lambda)

# Simulate from weibull distribution
weib <- function(n, alpha, beta){
  u <- runif(n, 0, 1)
  x <- (-(log(u)/alpha))^(1/beta)
  return(x)
}

# True weibull density with our parameterization
dweib <- function(x, alpha, beta){
  alpha*beta*(x^(beta-1))*exp(-alpha*x^beta)
}

# Simulation
a <- 3 # alpha
b <- 1 # beta

xw2 <- weib(1000, a, b) 
hist(xw2, breaks = 50, probability = T)
# truehist(xw2) # Alternative


# Check with distribution
xw <- seq(from = 0, to = 3, by = 0.01) # Uniform x's
yw <- dweib(xw, a,b) # Sample y
# plot(xw,yw, "l") # Alternative to lines(xw, yw)
truehist(xw2)
lines(xw, yw)


# Parameters as vectors
sample <- weib(10000, c(1,1), c(5,1))
truehist(sample)

# alpha = 1, beta = 5
extr1 <- sample[seq(1,length(sample), 2)] # Extract values
truehist(extr1)

# alpha = 1, beta = 1
extr2 <- sample[seq(2,length(sample), 2)]
truehist(extr2)




#Oppg B: Box-Muller transform --------------
n <- 10000
my <- 0
sigma <- 1

norm1 <- function(n, my, sigma){
  R = weib(n, 1/2, 2)
  theta = runif(n, 0, 2*pi)
  z = R*cos(theta)
  x= sigma*z+my
  return(x)
}

# Standard normal
xnorm <- norm1(n, my, sigma)
truehist(xnorm)

# Normal
xnorm2 <- norm1(n, my = 2, sigma = 3)
truehist(xnorm2)
