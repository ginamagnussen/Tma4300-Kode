# ---------- Utilities ------------ #
f_star <- function(x, alpha){
  return (((alpha-1)/2)*log(x^2) -x)
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
  
  x_1 <- matrix(n,1)
  x_2 <- matrix(n,2)
  
  # Until we have n realisations
  while (it <= n) {
    
    # Uniform sampling
    u_1 <- runif(1) * a_pluss
    u_2 <- runif(1) * b_pluss
    
    # Change of variables
    x_2_square <- 2 * u_1
    x_1_temp <- u_2 / u_1
    
    # Acceptance
    if (x_2_square <= f_star(x_1_temp, alpha)) {
      
      x_1[it] <- x_1_temp
      x_2[it] <- u_1
      
      # Increment
      it <- it + 1
      count <- count + 1
    }
    else {
      # Increment count
      count <- count + 1
    }
  }
  struct <- data.frame(x_1, x_2 , count)
  
  return (struct)
}