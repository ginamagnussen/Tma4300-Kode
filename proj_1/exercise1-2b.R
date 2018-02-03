# TMA4300 Exercise 1
# Problem B: b)

setwd("/home/shomea/g/ginama/V2018/git/Tma4300-Kode/proj_1")
source("sampleBeta.R")
library(MASS)
sample <- sampleBeta(2,3,10000)
truehist(sample)
