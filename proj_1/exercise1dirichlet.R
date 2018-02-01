# Problem B: Dirichlet distribution

setwd("/home/shomea/g/ginama/V2018/TMA4300 Computer Intensive Statistical Methods")
source("sampleGamma.R")

K <- 500
n <- 1
alpha <- runif(K)

zsample <- matrix(0,K,n)
dirsample <- matrix(0,1,K)
for (k in 1:K){
  zsample[k] <- sampleGamma(alpha[k], 1, n)
  dirsample[k] <- zsample[k]/(sum(zsample))
}
truehist(dirsample)


