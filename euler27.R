# Project Euler Problem 27: Quadratic Primes
#1. Take quadratic equation n^2 + an + b
#2. For each a and each b, determine number of consecutive primes with n = 0 to start

library(dplyr)

beg_time <- Sys.time()
n <- 100000

y <- function(a, b, q) {
  count_i <- 0
  n <- 0
  while (n <= length(q)) {
    z <- ((n ^ 2) + a*n + b)
    if (is.element(z, q)) {
      count_i <- count_i + 1
      n <- n + 1
    } else {
      break
    }
  }
  return(count_i)
}


q <- c(2)
i <- 3
j <- 1

while (i <= n) {
  while (j <= length(q)) {
    if ((i %% q[j]) == 0) {
      j <- 1
      break
    } else if (q[j] > sqrt(i)) {
      q <- append(q, i)
      j <- 1
      break
    } else {
      j <- j + 1
    }
  }
  j <- 1
  i <- i + 1
}

coeff1 <- 0
coeff2 <- 0
a <- -999
b <- 1
r_max <- 0

while (q[b] < 1000) {
  a <- -999
  while (a < 1000) {
    r <- y(a, q[b], q)
    if (r > r_max) {
      r_max <- r
      coeff1 <- a
      coeff2 <- q[b]
      #a <- a + 1
    }
    r <- y(a, -q[b], q)
    if (r > r_max) {
      r_max <- r
      coeff1 <- a
      coeff2 <- -q[b]
      a <- a + 1
    } else {
      a <- a + 1
    }
  }
  b <- b + 1
}


d <- coeff1 * coeff2
print(paste("The answer to Project Euler problem # 27 is:", d))


#print(q)

end_time <- Sys.time()
process_time <- end_time - beg_time
print(process_time)