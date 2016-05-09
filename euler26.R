# Project Euler Problem 27: Quadratic Primes
#1. Take quadratic equation n^2 + an + b
#2. For each a and each b, determine number of consecutive primes with n = 0 to start

library(dplyr)

beg_time <- Sys.time()

i <- 1
n <- 999
while (i <= n) {
  x <- (1 / i)
  print(x, digits = 22)
  i <- i + 1
}

print(paste("The answer to Project Euler problem # 26 is:"))

end_time <- Sys.time()
process_time <- end_time - beg_time
print(process_time)