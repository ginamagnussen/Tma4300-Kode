# TMA4300
# Bayesian model

# Birthdays
m <- 100000
stud <- 35
count <- 0

for (i in 1:m){
  bdays <- round(runif(stud)*365)
  if (sum(duplicated(bdays))>=1){
    count <- count + 1
  }
}

prob <- count/m


