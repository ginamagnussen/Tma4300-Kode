# Part 3, Ratio of Uniforms
setwd("/home/shomea/m/marcusae/Documents/git/Tma4300-Kode/proj_1")
source("sampleGammaRatioUniforms.R")


# ---------- Utilities ------------ #
f_star <- function(x, alpha){
    return (((alpha-1)/2)*log(x^2) -x)
}

n <- 1000

counts <- matrix(n,1)

# Generating realisations for set of alphas
count_it <- 0
for (alpha in 2:n) {
  
  out <- sampleGammaRatioUniforms(alpha, 1, n)
  count_it <- count_it + 1
  counts[count_it] <- out$count 

}

plot(2:n, counts)