#PROBLEM A: The coal-mining disater data

library(boot)
library(invgamma)


# 1)
data <- coal #Saves the coal data in a variable
size <- dim(data)
#Find the cumulative number of accidents
vec <- rep(1, size[1]-2)
cumDis <- cumsum(vec)
#Remove endpoints as these were not time of disaster
timeDis <- data[2:(size[1]-1), 1]
#Plotting the results
plot(timeDis, cumDis, main = "Cumulative number of disasters plotted against time", xlab = "Time of disaster in year", ylab = "Cumulative number of disaster")



#5) Tror ikke dette gir noe mening egnetlig "/
randomWalkT1 <- function(x, t, lambda0, lambda1, sig){
  
  x0 <- x[1]
  x1 <- x[2]
  t0 <- t[1]
  t1 <- t[2]
  t3 <- t[3]
  
  t1_new <- rnorm(1, mean = t1, sd = sig)
  x0_new <- lambda0*(t1_new - t0) #not correct find from the data!!!
  x1_new <- lambda1*(t2 - t1_new)
  
  ret <- c(t1,x0, x1)
  
  accprob = min(0, ((x0_new-x0)*log(lambda0)+(x1_new-x1)*log(lambda1)+(lambda1-lambda0)*(t1_new - t1)))
  u <- log(runif(1))
  if(u < accprob){
    t1 <- t1New
    ret <- c(t1,x0_new, x1_new)
  }
  
  return(t1)
}

CoalMiningMCMC <- function(x, t, lambda0, lambda1, beta){

  t0 <- t[1]
  t2 <- t[3]
  
  m <- 100
  
  lambda0_vec <- vector(mode = "numeric", length = (m+1))
  lambda1_vec <- vector(mode = "numeric", length = (m+1))
  beta_vec <- vector(mode = "numeric", length = (m+1))
  t1_vec <- vector(mode = "numeric", length = (m+1))
  x0_vec <- vector(mode = "numeric", length = (m+1))
  x1_vec <- vector(mode = "numeric", length = (m+1))
  
  lambda0_vec[1] <- lambda0
  lambda1_vec[1] <- lambda1
  beta_vec[1] <- beta
  t1_vec[1] <- t[2]
  x0_vec[1] <- x[1]
  x1_vec[1] <- x[2]
  
  for(i in 2:m){
    #Gibbs sampling, acceptence probability = 1
    lambda0_vec[i] <- rgamma(1, shape = (x0+2), scale = (1/beta_vec[i-1] + t1_vec[i-1] - t0)^(-1)) #sjekk rett parameterisering
    lambda1_vec[i] <- rgamma(1, shape = (x1+2), scale = (1/beta_vec[i-1] + t2 - t1_vec[i-1])^(-1)) #sjekk rett parameterisering
    beta_vec[i] <- rinvgamma(1, shape = 4, scale = (lambda0_vec[i-1] + lambda1_vec[i-1] + 1)) #sjekk rett parameterisering
    
    #Random walk
    temp <- randomWalkT1(x, t1_vec[i-1], lambda0_vec[i-1], lambda1_vec[i-1], sig) #sig is tuneing parameter
    t1_vec[i] <- temp[1]
    x0_vec[i] <- temp[2]
    x1_vec[i] <- temp[3]
  }
  
  iter <- seq(1, m+1)
  plot(iter, lambda0_vec, "l")
  plot(iter, lambda1_vec, "l")
  plot(iter, sig0_vec, "l")
  plot(iter, sig1_vec, "l")
  plot(iter, beta_vec, "l")
  plot(iter, t1_vec, "l")
  plot(iter, x0_vec, "l")
  plot(iter, x1_vec, "l")

  
}





#PROBLEM B: Bayesian image reconstruction

yGrid <- matrix(scan("C:/Users/Elisabeth/OneDrive/MTFYMA/4. Fysmat/Beregningskrevende/Oving2/image.txt", sep = " "), nrow = 85)

#1) through 3) were all solely theoretical

#4)

#(a)

#This function returns amount of equal ones / zeroes

equalNeighbour <- function(gridMat, i, j, boolian) {
  ret <- 0
  dimG <- dim(gridMat)
  x <- gridMat[i, j]
  if(boolian){ # checking neighbours from y to x
    x <- 1 - x
  }
  if((i>1) & (i < dimG[1]) & (j>1) & (j<dimG[2])){
    for(k in c(-1,1)){
      ret <- ret + (gridMat[i, j + k] == x)
      ret <- ret + (gridMat[i + k, j] == x)
    }
  } else if ((i==1) & (j > 1) & (j<dimG[2])) {
    ret <- (x == gridMat[i,j-1]) + (x == gridMat[i,j+1]) + (x == gridMat[i+1,j])
    
  } else if ((i>1) & (i<dimG[1]) & (j == 1)) {
    ret <- (x == gridMat[i-1,j]) + (x == gridMat[i+1,j]) + (x == gridMat[i,j+1])
    
  } else if ((i == dimG[1]) & (j > 1) & (j<dimG[2])) {
    ret <- (x == gridMat[i,j-1]) + (x == gridMat[i,j+1]) + (x == gridMat[i-1,j])
    
  } else if ((i>1) & (i<dimG[1]) & (j == dimG[2])) {
    ret <- (x == gridMat[i-1,j]) + (x == gridMat[i+1,j]) + (x == gridMat[i,j-1])
    
  } else if ((i==1) & (j == dimG[2])) {
    ret <- (x == gridMat[i,j-1]) + (x == gridMat[i+1,j])
    
  } else if ((i==dimG[1]) & (j == 1)) {
    ret <- (x == gridMat[i-1,j]) + (x == gridMat[i,j+1])
    
  } else if ((i==dimG[1]) & (j == dimG[2])) {
    ret <- (x == gridMat[i,j-1]) + (x == gridMat[i-1,j])
    
  }else if ((i==1) & (j ==1)) {
    ret <- (x == gridMat[i,j+1]) + (x == gridMat[i+1,j])
    
  }
  return(ret)
}

#(a)

updateRandomNode <- function(x, mu0, mu1, sig0, sig1, y, beta) { #gridMat is our x
  # Choosing specific node to work on
  gridSize <- dim(x)
  i <- sample(1:gridSize[1], 1)
  j <- sample(1:gridSize[2], 1)
  u <- runif(1) # Drawing random number between 0 and 1
  
  if(x[i,j]==0){
    muOld <- mu0
    muNew <- mu1
    sigOld <- sig0
    sigNew <- sig1
  }else{
    muOld <- mu1
    muNew <- mu0
    sigOld <- sig1
    sigNew <- sig0
  }
  
  logFromNormal <- (-2*log(sigNew)-(1/(2*sigNew^2)*(y[i,j]-muNew)^2)) - (-2*log(sigOld)-(1/(2*sigOld^2)*(y[i,j]-muOld)^2))
  # Acceptance probability for x
  accProb <- min(1,exp(beta*equalNeighbour(x,i,j, TRUE) - beta*equalNeighbour(x,i,j, FALSE)+logFromNormal)) 
  if (u < accProb){ 
    x[i,j] <- 1-x[i,j]
  }
  return(x)
}




#(b)
updateMu0 <- function(x, mu0, mu1, sig0, sig1, y) {
  n0 <- sum(x==0)
  n1 <- sum(x==1)
  mu0New <- rnorm(n = 1, mean = sum(y[x==0])/n0, sd = sqrt(sig0^2/n0))
  xsize <- dim(x)
  logQNew <- log(dnorm(mu0New, mean = sum(y[x==0])/n0, sd = sqrt(sig0^2/n0))) 
  logQOld <- log(dnorm(mu0, mean = sum(y[x==0])/n0, sd = sqrt(sig0^2/n0)))
  normP <- (-1/(2*sig0^2))*(sum((y[x==0] - mu0New)^2) - sum((y[x==0] - mu0)^2))
  u <- runif(1) # Drawring random number between 0 and 1
  if (mu0New < mu1) {
    accProb <- min(1, exp(log(impPri(x, mu0New, mu1, sig0, sig1, y) / impPri(x, mu0, mu1, sig0, sig1, y)) + normP + logQOld - logQNew))
  } else {
    accProb <- 0
  }
  if (u < accProb) {
    mu0 <- mu0New 
  }
  return(mu0)
}




#(c)
updateMu1 <- function(x, mu0, mu1, sig0, sig1, y) {
  n0 <- sum(x==0)
  n1 <- sum(x==1)
  mu1New <- rnorm(n = 1, mean = sum(y[x==1])/n1, sd = sqrt(sig1^2/n1))
  xsize <- dim(x)
  logQNew <- log(dnorm(mu1New, mean = sum(y[x==1])/n1, sd = sqrt(sig1^2/n1)))
  logQOld <- log(dnorm(mu1, mean = sum(y[x==1])/n1, sd = sqrt(sig1^2/n1))) 
  normP <- (-1/(2*sig1^2))*(sum((y[x==1] - mu1New)^2) - sum((y[x==1] - mu1)^2))
  u <- runif(1) # Drawring random number between 0 and 1
  if (mu0 < mu1New) {
    accProb <- min(1, exp(log(impPri(x, mu0, mu1New, sig0, sig1, y) / impPri(x, mu0, mu1, sig0, sig1, y)) + normP + logQOld - logQNew))
  }else{
    accProb <- 0
  }
  if (u < accProb) {
    mu1 <- mu1New 
  }
  return(mu1)
}




#(d)
impPri <- function(x, mu0, mu1, sig0, sig1, y) {
  ret <- 0
  if(sig0 < sig1) {
    ret <- ((mu1-mu0)^3 / (sig0^3*sig1^2)) * exp(-((mu1-mu0)/sqrt(sig0*sig1) + sqrt(sig1/sig0)))
  } else {
    ret <- ((mu1-mu0)^3 / (sig0^2*sig1^3)) * exp(-((mu1-mu0)/sqrt(sig0*sig1) + sqrt(sig0/sig1)))
  }
}

updateSig0 <- function(x, mu0, mu1, sig0, sig1, y, alpha, beta){
  # alpha << beta for large variance
  n0 <- sum(x==0)
  n1 <- sum(x==1)
  shape1 <- alpha + n0/2
  scale1 <- 1/(beta + (sum((y[x==0]-mu0)^2))/2)
  
  squaredSig0New <- rinvgamma(1, shape = shape1, scale = scale1)
  accProb <- min(1, exp(log(impPri(x, mu0, mu1, sqrt(squaredSig0New), sig1, y) / impPri(x, mu0, mu1, sig0, sig1, y)))) #+ newTerm)) #+ normP + logQOld - logQNew))
  
  u <- runif(1)
  if(u < accProb){
    sig0 <- sqrt(squaredSig0New)
  }
  return(sig0)
}



#e)
updateSig1 <- function(x, mu0, mu1, sig0, sig1, y, alpha, beta){
  # alpha << beta for large variance
  n0 <- sum(x==0)
  n1 <- sum(x==1)
  
  shape1 <- alpha + n1/2
  scale1 <- 1/(beta + (sum((y[x==1]-mu1)^2))/2)
  
  squaredSig1New <- rinvgamma(1, shape = shape1, scale = scale1)
  
  accProb <- min(1, exp(log(impPri(x, mu0, mu1, sig0, sqrt(squaredSig1New), y) / impPri(x, mu0, mu1, sig0, sig1, y))))
  
  u <- runif(1)
  
  if(u < accProb){
    sig1 <- sqrt(squaredSig1New)
  }
  return(sig1)
}


# Checking that we use the corret inverse-gamma
invGamPlot <- function(alpha, beta) {
  par(mfrow = c(2,1))
  x <- seq(from = 0, to = 3, by = 0.001)
  plot(x, (beta^(alpha))/gamma(alpha) * x^(-alpha-1) * exp(-beta/x), "l", col = "RED")
  plot(x, dinvgamma(x, shape = alpha, scale = beta), "l", col = "BLUE")
}




oneIter <- function(x, mu0, mu1, sig0, sig1, y, beta, invAlpha, invBeta) {
  ourDim <- dim(x)
  length <- ourDim[1]*ourDim[2]
  for (i in 1:length) {
    x <- updateRandomNode(x, mu0, mu1, sig0, sig1, y, beta)
  }
  mu0 <- updateMu0(x, mu0, mu1, sig0, sig1, y)
  mu1 <- updateMu1(x, mu0, mu1, sig0, sig1, y)
  sig0 <- updateSig0(x, mu0, mu1, sig0, sig1, y, invAlpha, invBeta)
  sig1 <- updateSig1(x, mu0, mu1, sig0, sig1, y, invAlpha, invBeta)
  
  l <- list(en = x, to = mu0, tre = mu1, fir = sig0, fem = sig1)
  return(l)
}



#5 --------------------------------------------------------------------

#Use one of these!
beta <- 0
#beta <- 0.6
#beta <-1


#Run this to test our algorithm
output <- iterMCMC(-1, 2, 0.5, 0.5, 1, yGrid)

iterMCMC <- function(mu0, my1, sig0, sig1, beta, y) {
  #invAlpha << invBeta for large variance
  invAlpha <- 1
  invBeta <- 100
  
  size <- dim(y)
  # Initialising x
  r <- size[1]
  c <- size[2]
  x <- matrix(0, r, c)
  x <- apply(x, c(1,2), function(x) sample(c(0,1),1))
  n = 2000
  vec <- vector(mode = "numeric", length = n)
  mu0_vec <- vector(mode = "numeric", length = n)
  mu1_vec <- vector(mode = "numeric", length = n)
  sig0_vec <- vector(mode = "numeric", length = n)
  sig1_vec <- vector(mode = "numeric", length = n)
  #l <- list("x" = x, "mu0" = mu0, "mu1" = mu1, "sig0" = sig0, "sig1" = sig1)
  temp <- list(en = x, to = mu0, tre = mu1, fir = sig0, fem = sig1)
  
  test <- matrix(0, nrow = r, ncol = c)
  
  burnin <- 300
  
  average <- vector(mode = "numeric", length = (n-burnin))
  
  #install.packages("tictoc")
  #library(tictoc)
  par(mfrow = c(2, 2))
  tic()
  for(i in 1:n){
    if(i == 1){
      print(i)
      image(seq(1,size[1]), seq(1,size[2]), yGrid)
    }
    if(is.element(i, c(20, seq(2,n, n/5)))){
      print(i)
      image(seq(1,size[1]), seq(1,size[2]), x)
    }
    
    x <- temp$en
    mu0_vec[i] <- temp$to
    mu1_vec[i] <- temp$tre
    sig0_vec[i] <- temp$fir
    sig1_vec[i] <- temp$fem
    
    vec[i] <- sum(x==0)
    
    temp <- oneIter(x, mu0_vec[i], mu1_vec[i], sig0_vec[i], sig1_vec[i], y, beta, invAlpha, invBeta)
    
    
    if(i > burnin){ #removing burnin period
      test <- test + x
      average[i-burnin] <- mean(x)
    }
    
    if(i == n){
      x <- temp$en
      mu0_vec[i+1] <- temp$to
      mu1_vec[i+1] <- temp$tre
      sig0_vec[i+1] <- temp$fir
      sig1_vec[i+1] <- temp$fem
    }
  }
  toc()
  
  vec <- c(vec, sum(x==0))
  
  # Mean values E[x_ij|y]
  test <- test/(n-burnin)
  image(seq(1:size[1]), seq(1:size[2]), test)
  
  #Posterior marginal most probable value for each x_ij
  image(seq(1:size[1]), seq(1:size[2]), round(test))
  print("Estimated posterior, fraction of x_ij = 1")
  print(mean(average))
  
  #Variance, Confidence interval
  variance <- var(average)
  tqLow <- qt(0.05, (85*89-1))
  tqUp <- qt(0.95, (85*89-1))
  ConfInt <- c(mean(average)+tqLow*variance, mean(average)+tqUp*variance)
  
  #Expected fraction of nodes with x_ij = 1
  print("Expected fraction of nodes with x_ij = 1")
  print(mean(average))
  
  #Credible interval for x_ij = 1
  average <- sort(average)
  print(quantile(average, c(0.05, 0.95)))
  min <- round(0.05*(n-burnin))
  max <- round(0.95*(n-burnin))
  CI <- c(average[min], average[max]) #Ble dette riktig??
  print("90% - Credible interval for x_ij == 1")
  print(CI)
  
  #Printing how the values differ
  par(mfrow = c(2,2))
  plot(seq(1, n+1), mu0_vec, "l", main = "Value of mu0 at iteration i")
  
  plot(seq(1, n+1), mu1_vec, "l", main = "Value of mu1 at iteration i")
  
  plot(seq(1, n+1), sig0_vec, "l", main = "Value of sig0 at iteration i")
  
  plot(seq(1, n+1), sig1_vec, "l", main = "Value of sig1 at iteration i")
  
  #Plotting
  par(mfrow = c(2,2))
  plot(seq(1, n+1), vec, "l", main = "Elements in x == 0 on average")

  #image(seq(1:size[1]), seq(1:size[2]), x)
  
  #Plotting histogram for y|x which should be normal
  hist(y[x==0])
  abline(v = mu0_vec[n+1], col = "RED")
  abline(v = mu0_vec[n+1] + sig0_vec[n+1], col = "BLUE", lty = 2)
  abline(v = mu0_vec[n+1] - sig0_vec[n+1], col = "BLUE", lty = 2)
  hist(y[x==1])
  abline(v = mu1_vec[n+1], col = "RED")
  abline(v = mu1_vec[n+1] + sig1_vec[n+1], col = "BLUE", lty = 2)
  abline(v = mu1_vec[n+1] - sig1_vec[n+1], col = "BLUE", lty = 2)
  
  ret<- list(x, mu0_vec, mu1_vec, sig0_vec, sig1_vec)
  
  #Looking at convergence
  par(mfrow = c(2,2))
  t <- seq(burnin, (n+1))
  
  #x==0
  plot(t, vec[burnin:(n+1)], "l", main = "Elements in x==0 at iteration i, after burnin")
  accRVec <- 1 - mean(duplicated(vec[burnin:(n+1)]))
  print("Acceptence rate, #x==0:")
  print(accRVec)
  acf(vec)
  
  #mu0
  par(mfrow = c(2,2))
  plot(t, mu0_vec[burnin:(n+1)], "l", main = "Value of mu0 at iteration i, after burnin")
  accRMu0 <- 1 - mean(duplicated(mu0_vec[burnin:(n+1)]))
  print("Acceptence rate, mu0:")
  print(accRMu0)
  acf(mu0_vec)
  truehist(mu0_vec[burnin:(n+1)])
  lines(density(mu0_vec))
  
  #mu1
  par(mfrow = c(2,2))
  plot(t, mu1_vec[burnin:(n+1)], "l", main = "Value of mu1 at iteration i, after burnin")
  accRMu1 <- 1 - mean(duplicated(mu1_vec[burnin:(n+1)]))
  print("Acceptence rate, mu1:")
  print(accRMu1)
  acf(mu1_vec)
  truehist(mu1_vec[burnin:(n+1)])
  lines(density(mu1_vec))
  
  #sig0
  par(mfrow = c(2,2))
  plot(t, sig0_vec[burnin:(n+1)], "l", main = "Value of sig0 at iteration i, after burnin")
  accRSig0 <- 1 - mean(duplicated(sig0_vec[burnin:(n+1)]))
  print("Acceptence rate, sig0:")
  print(accRSig0)
  acf(sig0_vec)
  truehist(sig0_vec[burnin:(n+1)])
  lines(density(sig0_vec))
  
  #sig1
  par(mfrow = c(2,2))
  plot(t, sig1_vec[burnin:(n+1)], "l", main = "Value of sig1 at iteration i, after burnin")
  accRSig1 <- 1 - mean(duplicated(sig1_vec[burnin:(n+1)]))
  print("Acceptence rate, sig1:")
  print(accRSig1)
  acf(sig1_vec)
  truehist(sig1_vec[burnin:(n+1)])
  lines(density(sig1_vec))
  
  return(ret)
}

