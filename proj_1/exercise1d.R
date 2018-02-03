# Sample from generic gamma distribution 
setwd("/home/shomea/g/ginama/V2018/git/Tma4300-Kode/proj_1")
source("sampleGamma.R")
library(MASS)

# Initialize
alpha <- c(1,2,4)
beta <- c(0.2,1,6)
n <- 100000

# Sample
xSample1 <- sampleGamma(alpha[1], beta[1], n)
xSample2 <- sampleGamma(alpha[2], beta[2], n)
xSample3 <- sampleGamma(alpha[3], beta[3], n)

# Plot
#frame <- data.frame(xSample1, xSample2, xSample3)
# ggplot(frame) + geom_histogram(aes(xSample1, y = ..density..), col = "black", binwidth = 0.2)  + geom_histogram(aes(xSample2, ..density..), col = "black", fill = "red", binwidth = 0.1)
truehist(xSample1, main = "Gamma sampling, alpha = and beta = ")
truehist(xSample2, main = "Gamma sampling, alpha = and beta = ")
truehist(xSample3, main = "Gamma sampling, alpha = and beta = ")

# Check
means <- c(mean(xSample1), mean(xSample2), mean(xSample3))
truemean <- alpha*beta

variances <- c(var(xSample1), var(xSample2), var(xSample3))
truevar <- alpha*beta^2

frame <- data.frame(means, truemean, variances, truevar)
print(frame)



