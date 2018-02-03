# Problem B: Dirichlet distribution
setwd("/home/shomea/g/ginama/V2018/git/Tma4300-Kode/proj_1")
source("sampleGamma.R")

# Problem B: Dirichlet distribution
library(MASS)
#library(Compositional)

# ----- Utilities --------#
meanDir <- function(alpha, K){
  sum_alpha = sum(alpha)
  mean_out <-  matrix(0, K, 1)
  for (it in 1:K){
    mean_out[it] <- alpha[it] / sum_alpha
  }  
  return (mean_out)
}

covDir <- function(alpha, K){
  alpha_0 <- sum(alpha)
  cov_out <- matrix(0, K, K)
  print(alpha_0)
  for (i in 1:K) {
    for (j in 1:K){
      if (i != j) {
        cov_out[i,j] <- -(alpha[i] * alpha[j]) / (alpha_0^2 * (alpha_0 + 1))
      }
    }
  }
  return (cov_out)
}

n <- 10000
K <- 3
alpha <- 1:K

zSample <- matrix(0,n,K)
dirSample <- matrix(0,n,K)
dirMean <- matrix(0,K,1)

for (i in 1:n) {
  for (j in 1:K){
    zSample[i, j] <- sampleGamma(alpha[j], 1, 1)
  }
  for (j in 1:K){
    dirSample[i, j] <- zSample[i,j]/(sum(zSample[i,]))
  }
}

for (j in 1:K) {
  dirMean[j] <- mean(dirSample[j,])
}

# Verify marginal distributions are beta
xUni <- seq(0,1,1/n)
betaSamples_1 <- dbeta(xUni, alpha[1], sum(alpha)-alpha[1])
betaSamples_2 <- dbeta(xUni, alpha[2], sum(alpha)-alpha[2])
betaSamples_3 <- dbeta(xUni, alpha[3], sum(alpha)-alpha[3])


# Plotting
# par(mfrow=c(1,3))
truehist(dirSample[,1])
lines(xUni, betaSamples_1, col = "red", lwd = 2)

truehist(dirSample[,2])
lines(xUni, betaSamples_2, col = "red", lwd = 2)

truehist(dirSample[,3])
lines(xUni, betaSamples_3, col = "red", lwd = 2)
