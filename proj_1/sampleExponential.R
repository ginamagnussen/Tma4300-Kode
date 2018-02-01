# Exponential distribution

sampleExponential <- function(lambda, n){
  x <- -(1/lambda)*log(runif(n))
  return(x)
}
