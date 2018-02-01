# TMA4300
# Exercise 1
setwd("/home/shomea/m/marcusae/Documents/git/Tma4300-Kode/proj_1")
source("sampleGammaRejection.R")
# PROBLEM A, Part 1

library(MASS)
library(ggplot2)
n <- 3000
alpha <- 0.5
x <- sort(runif(n,0.05,8))

# Rejection sampling, gamma
xsample <- sampleGammaRejection(alpha, n)
truehist(xsample)
# ggplot(data.frame(xsample), aes(xsample)) + geom_histogram()

#Check
gammaf <- function(alpha,x){
  res <- (1/gamma(alpha))*(x^(alpha-1))*exp(-x)
  return(res)
}
y <- gammaf(alpha, x)
lines(x,y)


#----------------------------------------------------------

# Probability integral transform
xsample2 <- gsample(n, alpha) 
truehist(xsample2)


# Check sampling

gfunc <- matrix(0,n,1)

for (i in 1:n){
  if(x[i] <= 0){
    gfunc[i] <- 0
  }
  else if (x[i] < 1) {
    gfunc[i] <- ((exp(1)*alpha)/(exp(1)+alpha))*(x[i]^(alpha-1))
  }
  else {
    gfunc[i] <- ((exp(1)*alpha)/(exp(1)+alpha))*exp(-x[i])
  }
}

dataf <- data.frame(x, gfunc)
#ggplot(dataf, aes(x,gfunc)) + geom_point()
points(x,gfunc)







