# TMA4300
# Exercise 1
setwd("/home/shomea/m/marcusae/Documents/git/Tma4300-Kode/proj_1")
source("sampleGammaRejection.R")
# PROBLEM A, Part 1

library(MASS)
n <- 3000
alpha <- 0.5

xsample <- sampleGammaRejection(alpha, n)

truehist(xsample)
