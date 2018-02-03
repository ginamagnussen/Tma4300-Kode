# TMA4300 Computer intensive statistical methods
# Interactive 2, 19.01.18 + material added later

# PART A:
#1
x <- seq(from = 0, to = 2, by = 0.01)
y <- dbeta(x, 2, 2)
plot(x,y, type = "l")
plot(x,y)
lines(x,y)

#2, 4
alpha <- 2
beta <- 2

betad <- function(alpha, beta){
  fin <- 0
  while(fin == 0){
    x <- runif(1)
    acc <- ((1-alpha)/(2-alpha-beta))^(1-alpha)*((1-beta)/(2-alpha-beta))^(beta-1)*(x^(alpha-1))*(1-x)^(beta-1)
    u <- runif(1)
    if (u <= acc){
      fin <- 1
    }
  }
  return(x)
}

#3 Average no. of proposals needed to generate one realisation = c 
# (Geometric distribution)

#--------------------------------------------------------------
# Problem B: Sampling using known relations

# GEOMETRIC DISTRIBUTION
# Sampling using the 'discrete distribution', F_(i-1), F_i ...
geomcdf <- function(x,p){
  return(1-(1-p)^x)
}


p <- 1/2
xval <- seq(from = 1, to = 10, by = 1)
cdfval <- geomcdf(xval, p)


u <- runif(1)
geomSample <- function(cdfv){
  i <- 1
  while (u >= cdfval[i]){
    i <- i + 1
  }
  return(i)
}

geomSample(cdfval)

# Sampling using known relations
# Know that geometric distribution is special case of negative binomial distribution.
# and binomial distributions are the sum of bernoulli trials
# That means: Sample bernoulli trials until one success.
p1 <- 1/2

geomSampleRel <- function(p){
  done <- 0
  count <- 0
  while (done == 0){
    u1 <- runif(1)
    print(u1)
    if (u1 <= p1){
      done <- 1
    }
    count <- count + 1
  }
  return(count)
}



#-------------------------------------
# Problem B

# 1) Geometric distribution sampling
cdff <- function(x,p){
  1-(1-p)^x
}

var <- 10
x <- seq(from = 1, to = var, by = 1)
p <- 0.2
Fun <- cdff(x,p)

n <- 2
u <- runif(n)
i <- 1
for (j in 1:n){
  while (u[j] >= Fun[i]) {
    i <- i + 1
  }
}
i


