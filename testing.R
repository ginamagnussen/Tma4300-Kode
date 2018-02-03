# TMA4300 Computer intensive statistical methods
# Testing

# Approximate pi -------------------------
n <- 1000000
m <- 0 
i <- 1

while(i < n){
  x <- runif(n = 1, min = 0, max = 1)
  y <- runif(n = 1,min = 0, max = 1)
  if (x^2 + y^2 < 1){
    m <- m + 1
  }
  i = i + 1 
}

est = 4*(m/n) 
est

# Plot, seed, runif

par(mfrow=c(1,1), mar=c(4,4,1,0), las=1, cex.lab=1.2, cex.axis=1.1, lwd=1.6)
set.seed(2118735)
a = runif(10)
plot(a[1:9], a[2:10], type="o", xlab="", ylab="", xlim=c(0,1), ylim=c(0,1))
plot(1,2)



#-----------------------------------------------
fstar <- function(x, alpha){
  return(x^(alpha-1)*exp(-x))
}
alph <- 2 
x <- seq(from = 0, to = 10, by = 0.01)
y <- fstar(x, alph)
plot(x,y, "l")

yb <- (x^2)*fstar(x,alph)
plot(x,yb, "l")



library(ggplot2)
dta <- data.frame(x = c(1.3,2,3,4,5), y = c(1,2.6,3,4,5))
ggplot(data = dta, aes(x,y)) + geom_point(aes(colour = "cyl")) 
