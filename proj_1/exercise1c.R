# Part 3, Ratio of Uniforms
setwd("/home/shomea/g/ginama/V2018/git/Tma4300-Kode/proj_1")
source("sampleGammaRatioUniforms.R")
library(ggplot2)

# ---------- Utilities ------------ #
n <- 1000 # No. of samples
alphamax <- 10
counts <- matrix(0,1,alphamax-1)

# Generating realisations for set of alphas
count_it <- 0
for (alpha in 2:alphamax) {
  
  out <- sampleGammaRatioUniforms(alpha, n)
  count_it <- count_it + 1
  counts[count_it] <- out$count[1] 
  
}

frame <- data.frame(alpha = 2:alphamax, counts = counts)
plot(2:alphamax, counts, main = "Rejection sampling: Number of iterations to achieve 1000 samples", xlab = "Alpha", ylab = "Counts", col = "red")
