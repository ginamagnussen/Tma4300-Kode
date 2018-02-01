# Sample gamma with any alpha and beta
setwd("/home/shomea/g/ginama/V2018/TMA4300 Computer Intensive Statistical Methods")
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
