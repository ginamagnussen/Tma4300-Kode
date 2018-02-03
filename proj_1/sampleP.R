setwd("/home/shomea/g/ginama/V2018/git/Tma4300-Kode/proj_1")
source("importanceSample.R")

findIndex <- function(vec){
  for (i in 1:length(vec)) {
    if (vec[i] == 1) {
      return (i)
    }
  }
  return(-1)
}

calcP <- function(weights, N, K) {
  
  # Choose random season
  u <- runif(1)
  p <- c(0.25, 0.5, 0.75, 1.0)
  out <- importanceSample(p, 1)
  index <- findIndex(out)
  temp <- weights[index] * factorial(K[index]) / factorial(K[index]-N[index]) * 1 / K[index]^N[index]
  print(temp)
  return (1 - temp)
}

samplePosterior <- function(p, students, draws, daysInSeason) {
  
  post <- matrix(0, draws, 1)
  out <- matrix(1, length(p),1)
  
  # For efficiency
  #p <- sort(p, ndex.return = FALSE)
  
  for (i in 1:draws){
    out <- importanceSample(p, students)
    post[i] <- calcP(p, out, daysInSeason)
  }
  print(out)
  
  return (post)
}

computePosteriorMeans <- function(p, students, draws, daysInSeason) {
  
  post <- samplePosterior(p, students, draws, daysInSeason)
  
  return (post)
}
