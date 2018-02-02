# Part 3, Ratio of Uniforms
#setwd("/home/shomea/m/marcusae/Documents/git/Tma4300-Kode/proj_1")
source("sampleGammaRatioUniforms.R")

# ---------- Utilities ------------ #
# f_star <- function(x, alpha){
#   return (((alpha-1)/2)*log(x^2) -x)
# }

n <- 1000 # No. of samples
alphamax <- 200
counts <- matrix(0,1,alphamax-1)

# Generating realisations for set of alphas
count_it <- 0
for (alpha in 2:alphamax) {
  
  out <- sampleGammaRatioUniforms(alpha, n)
  count_it <- count_it + 1
  counts[count_it] <- out$count[1] 
  
}

library(ggplot2)
frame <- data.frame(alpha = 2:alphamax, counts = counts)
#ggplot(data = frame, aes(alpha,counts)) + geom_point(colour = "red")
plot(2:alphamax, counts)
