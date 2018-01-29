# Sample from generic gamma distribution 
setwd("/home/shomea/m/marcusae/Documents/git/Tma4300-Kode/proj_1")
source("sampleGamma.R")

alpha <- 10
beta <- 2
n = 1000

xSample <- sampleGamma(alpha, beta, n)

hist(xSample, xlim = range(0:200))
