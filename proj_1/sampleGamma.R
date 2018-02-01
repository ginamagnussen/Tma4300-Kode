# Sample gamma with any alpha and beta
setwd("/home/shomea/m/marcusae/Documents/git/Tma4300-Kode/proj_1")
source("sampleGammaRatioUniforms.R")
source("sampleGammaRejection.R")

# ------------- Main function -----#
sampleGamma <- function(alpha, beta, n) {
  
  if (alpha > 1) {
    xFrame <- sampleGammaRatioUniforms(alpha,n)
    xSample <- xFrame$xSample
    
  }
  else if ( alpha < 1) {
    xSample <- sampleGammaRejection(alpha, n)
  }
  else {
    xSample <- sampleExponential(lambda = beta, n)
  }
  
  return (beta*xSample)
}
