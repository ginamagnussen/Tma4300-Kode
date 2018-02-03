# -------------- Importance sampling -----------------# 
# Input: 
# p : Sorted list of probabilities summing to unity.
# N : Number of draws
# Output:
# out: vector of draws corresponding to each interval

importanceSample <- function(p, N) {
  
  len_p <- length(p)
  
  # Compute cumulative sum
  cumSum <- matrix(0,len_p, 1)
  cumSum[1] <- p[1]
  for (i in 2:len_p) {
    cumSum[i] <- cumSum[i-1] + p[i]
  }
  
  u <- runif(N)
  
  # Increment output draws
  out <- matrix(0, len_p, 1)
  for (i in 1:N) {
    for (j in 1:len_p) {
      if (u[i] < cumSum[j]) {
        out[j] <- out[j] + 1
        break
      }
    }
  }
  return (out)
}