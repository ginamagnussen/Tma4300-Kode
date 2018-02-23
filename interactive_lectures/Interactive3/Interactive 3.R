# TMA4300 Computer intensive statistical methods
# PART 2: MCMC
# Interactive 3

# Problem A: MCMC for a toy problem
# 2)
# n = no. of steps
i <- 100 
alpha <- 0.5

mchain <- function(i){
  
  # FOR LOOP
  # sim <- rep(0,n)
  # for (j in 1:n){
  #   rand <- runif(1)
  #   if (rand < alpha){
  #     sim[j] <- 1
  #   }
  # }
  
  # VECTOR
  rand <- runif(i)
  sim <- rand < alpha
  return(sim)
}

# Check
sum(mchain(n)*1)




# 3)




# PART B: MCMC and time reversibility




