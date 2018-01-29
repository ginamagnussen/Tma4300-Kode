# ---------- Utilities ------------ #
f_star <- function(x1, x2, alpha){
  return ((alpha-1)*(log(x2) - log(x1)) - exp(log(x2) - log(x1)))
}

sampleGammaRatioUniforms <- function(alpha, n=1) { 
  
  # Determine bounds on a log scale
  a_pluss <- (alpha-1)/2 * log((alpha-1)*exp(-1))
  a_minus <- 0
  b_pluss <- (alpha+1)/2 * log((alpha+1)*exp(-1))
  b_minus <- 0
  
  # Iterations
  count <- 1
  it <- 1
  
  xSample <- matrix(n,1)
  
  # Until we have n realisations
  while (it <= n) {
    
    # Uniform sampling
    log_u_1 <- log(runif(1)) + a_pluss
    log_u_2 <- log(runif(1)) + b_pluss
    
    # Change of variables
    x_2 <- log_u_1
    x_1 <- log_u_2 - log_u_1
    
    # Acceptance
    if (2*x_2 <= f_star(x1, x2, alpha)) {
      
      xSample[it] <- x_1 - log_u_1
      
      # Increment
      it <- it + 1
      count <- count + 1
    }
    else {
      # Increment count
      count <- count + 1
    }
  }
  struct <- data.frame(xSample , count)
  
  return (struct)
}