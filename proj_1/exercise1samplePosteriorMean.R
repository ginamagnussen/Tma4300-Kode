# Exercise C.3.b
setwd("/home/shomea/g/ginama/V2018/git/Tma4300-Kode/proj_1")
source("sampleP.R")

students <- 35
draws <- 100

spring <- 92
summer <- 92 
autumn <- 91
winter <- 90

daysInSeason <- matrix(1, 4, 1)
daysInSeason[1] <- spring
daysInSeason[2] <- summer
daysInSeason[3] <- autumn
daysInSeason[4] <- winter

daysYear <- 365

p1 <- 92 / 365
p2 <- 92 / 365
p3 <- 91 / 365
p4 <- 90 / 365

p <- matrix(1, 4, 1)
p[1] <- p1
p[2] <- p2
p[3] <- p3
p[4] <- p4

#means <- computePosteriorMeans(p, students, draws, daysInSeason)
#print(means)

N <- matrix(1, 4, 1)

N[1] <- 9
N[2] <- 9
N[3] <- 9
N[4] <- 8

print(calcP(p, N,daysInSeason))
