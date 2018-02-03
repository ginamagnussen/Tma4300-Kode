# Rejection sampling

# # ---------Utilities -----------# 
gsample <- function(n,alpha){
  c <- (exp(1)*alpha)/(exp(1)+alpha)
  u <- runif(n)
  
  x1 <- (u*(alpha/c))^(1/alpha)
  x2 <- -log((1/alpha+1/(exp(1))-u/c))
  
  x <- matrix(0,n,1)
  
  for (i in 1:n){
    if(u[i]>= (c/alpha)){
      x[i] <- x2[i]
    }
    else {
      x[i] <- x1[i]
    }
  }
  return(x)
}


acceptanceProb <- function(x,alpha){
  if (x < 1) {
    return (exp(-x))
  }
  return (x^(alpha-1))
}

# -------------- Main function ---------------#
sampleGammaRejection <- function(alpha, n) {
  xsample <- matrix(0,n,1)
  for (j in 1:n){
    fin <- 0
    while (fin == 0){
      xsample[j] <- gsample(1,alpha)
      acc <- acceptanceProb(xsample[j], alpha) # Acceptance prob
      u <- runif(1)
      if (u <= acc){
        fin = 1
      }
    }
  }
  return (xsample)
}