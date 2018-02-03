# Sample beta distribution
setwd("/home/shomea/g/ginama/V2018/git/Tma4300-Kode/proj_1")
source("sampleGamma.R")

sampleBeta <- function(alpha, beta, n){
  Y <- sampleGamma(alpha, beta = 1,n)
  Z <- sampleGamma(alpha = beta, beta = 1, n)
  xsample <- Y/(Y+Z)
}

