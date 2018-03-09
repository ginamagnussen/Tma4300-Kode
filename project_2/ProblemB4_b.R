fcount <- function(xmat, ymat){
  
  rows <- dim(xmat)[1]
  cols <- dim(xmat)[2]
  count <- 0
  for (i in 1:rows){
    for(j in 1:cols){
      if (xmat[i,j] == 0){
        count <- count + 1
        yval[count] <- ymat[i,j]
      }
    }
  }
  ysum <- sum(yval)
  res <- data.frame(count, ysum)
  return(res)
}

  
res <- fcount(xmat,ymat)
N_0 <- res$count
ysum <- res$ysum
            
            
muzero <- function(N_0, ysum, sigma_0){
  mu_0 <- rnorm(1, ysum/N_0, (sigma_0^2)/N_0)
  return(mu_0)
}