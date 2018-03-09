# Count number of 0s or ones in x
# and return sum of corresponding values in y
fcount <- function(x, y, bool){
  nrows <- dim(x)[1]
  ncols <- dim(x)[2]
  count <- 0
  yval = 0
  for (i in 1:nrows){
    for(j in 1:ncols){
      if (x[i,j] == bool){
        count <- count + 1
        yval[count] <- y[i,j]
      }
    }
  }
  ysum <- sum(yval)
  res <- data.frame(count, ysum)
  return(res)
}
# b) For mu_0
res0 <- fcount(x,y,0)
N_0 <- res0$count
ysum0 <- res0$ysum
mu0_old <- mu_0

## Acceptance probability
# ---- Utilities --- #

ftheta <- function(mu_0, mu_1, sigma_0, sigma_1){
  if (sigma_0 <= sigma_1 && mu_0 < mu_1){
    res <- (((mu_1-mu_0)^3)/(sigma_0^3*sigma_1^2))*exp(-(((mu_1-mu_0)/(sqrt(sigma_0*sigma_1)))+sqrt(sigma_1/sigma_0)))
  }
  else if (sigma_0 > sigma_1 && mu_0 < mu_1){
    res <- (((mu_1-mu_0)^3)/(sigma_0^2*sigma_1^3))*exp(-(((mu_1-mu_0)/(sqrt(sigma_0*sigma_1)))+sqrt(sigma_1/sigma_0)))
  }
  return(res)
}

fygivenxphi <- function(val, mu, sigma){
  return(dnorm(val, mu, sigma))
}

# --- Main---- #
muzero <- function(N_0, ysum0, sigma_0, mu0_old){
  # Propose random new mu0
  mu0_new <- rnorm(1, ysum0/N_0, sqrt((sigma_0^2)/N_0))
  Q_old <- dnorm(mu0_old, ysum0/N_0, sqrt((sigma_0^2)/N_0), log = TRUE)
  Q_new <- dnorm(mu0_new, ysum0/N_0, sqrt((sigma_0^2)/N_0), log = TRUE)
  f_old <- log(ftheta(mu0_old, mu_1, sigma_0, sigma_1)*fygivenxphi(mu0_old, mu_0, sigma_0))
  f_new <- log(ftheta(mu0_new, mu_1, sigma_0, sigma_1)*fygivenxphi(mu0_new, mu_0, sigma_0))
  
                
  # NB: Ratio on log scale
  ratio <- (f_new*Q_old)/(f_old*Q_new)
  acc <- min(0, ratio) # on log scale
  u <- log(runif(1))
  if (u < acc){
    res <- mu0_new
  } else {
    res <- mu0_new
  }
  return(res)
}

test0 <- muzero(N_0, ysum0, sigma_0, mu0_old)
test0


# For mu_1
res <- fcount(x,y,1)
N_1 <- res$count
ysum1 <- res$ysum
mu1_old <- mu_1

muone <- function(N_1, ysum1, sigma_1, mu1_old){
  # Propose random new mu0
  mu1_new <- rnorm(1, ysum1/N_1, sqrt((sigma_1^2)/N_1))
  Q_old <- dnorm(mu1_old, ysum1/N_1, sqrt((sigma_1^2)/N_1), log = TRUE)
  Q_new <-dnorm(mu1_new, ysum1/N_1,sqrt((sigma_1^2)/N_1), log = TRUE)
  f_old <- log(ftheta(mu_0, mu1_old,sigma_0, sigma_1)*fygivenxphi(mu1_old, mu_1, sigma_1))
  f_new <-  log(ftheta(mu_0, mu1_new,sigma_0, sigma_1)*fygivenxphi(mu1_new, mu_1, sigma_1))
    
  # NB: Ratio on log scale
  ratio <- (f_new*Q_old)/(f_old*Q_new)
  acc <- min(0, ratio) # on log scale
  u <- log(runif(1))
  if (u < acc){
    res <- mu1_new
  } else {
    res <- mu1_new
  }
  return(res)
}

test1 <- muone(N_1, ysum1, sigma_1, mu1_old)
test1