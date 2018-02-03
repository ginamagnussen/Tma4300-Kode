# Exercise C.3.a
setwd("/home/shomea/g/ginama/V2018/git/Tma4300-Kode/proj_1")
source("importanceSample.R")

m <- 10000
N <- 1000

p1 <- 0.2
p2 <- 0.3 
p3 <- 0.25 
p4 <- 0.25
p <- matrix(1, 4, 1)
p[1] <- p1
p[2] <- p2
p[3] <- p3
p[4] <- p4

out <- importanceSample(p, m)

print(out)
