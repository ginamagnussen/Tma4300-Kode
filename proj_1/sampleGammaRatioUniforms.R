# ---------- Utilities ------------ #
f_star <- function(x, alpha){
  return (((alpha-1)/2)*x -exp(x)/2)
}

sampleGammaRatioUniforms <- function(alpha, n) { 
  
  # Determine bounds on a log scale
  #log_a_pluss <- (alpha-1)/2 * log((alpha-1)*exp(-1))
  log_a_pluss <- (alpha-1)/2 * (log(alpha-1)-1)
  # log_a_pluss <- log(sqrt((alpha-1)^(alpha-1)*exp(1-alpha)))
  #log_a_minus <- -Inf
  log_b_pluss <- (alpha+1)/2 * log((alpha+1)*exp(-1))
  # log_b_pluss <- log(sqrt((alpha+1)^(alpha+1)*exp(-(alpha+1))))
  #log_b_minus <- -Inf # log of both limits...
  
  # Iterations
  count <- 1
  it <- 1
  
  xSample <- matrix(0,n,1)
  
  # Until we have n realisations
  while (it <= n) {
    
    # Uniform sampling
    log_u_1 <- log(runif(1)) + log_a_pluss
    log_u_2 <- log(runif(1)) + log_b_pluss
    
    # Change of variables
    # x_2 <- log_u_1
    # x_1 <- log_u_2 - log_u_1
    log_x <- log_u_2 - log_u_1 #log(u2/u1)
    
    # Acceptance
    print(log_u_1 - f_star(log_x, alpha)); flush.console()
    if (log_u_1 <= f_star(log_x, alpha)) {
      
      xSample[it] <- log_x
      
      # Increment
      it <- it + 1
      print(log_u_1 - f_star(log_x, alpha)); flush.console()
      count <- count + 1
    }
    else {
      # Increment count
      count <- count + 1
    }
  }
  struct <- data.frame(xSample = exp(xSample) , count)
  
  return (struct)
}



