# Project Euler Problem 24: Lexicographic Numbers
#1. Determine which iteration you want to display (one millionth)
#2. Determine where to fix each number in the vector

library(dplyr)

beg_time <- Sys.time()
n <- 1000000

i <- 1
j <- 0
a <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
b <- c()

while (i <= length(a)) {
  print(a)
  print(b)
  y <- factorial(length(a) - 1)
  x <- ceiling((n - j) / y)
  if (x == 0) {
    b <- append(b, a[1])
    a <- a[-1]
  } else {
    b <- append(b, a[x])
    a <- a[-x]
  }
  j <- j + (x * y) - y
}

print(b)

end_time <- Sys.time()
process_time <- end_time - beg_time
print(process_time)