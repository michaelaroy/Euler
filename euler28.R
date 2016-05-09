# Project Euler Problem 28: Number Spiral Diagonals
#1. Build matrix starting with number 1 in center until all dimensions are filled
#2. Sum diagonals

library(dplyr)

beg_time <- Sys.time()
n <- 1001

diag <- function(n, M) {
  a <- 1
  b <- 1
  x <- 0
  y <- 0
  q <- ceiling(n / 2)
  while (a <= n) {
    x <- x + M[a, b]
    a <- a + 1
    b <- b + 1
  }
  a <- 1
  b <- n
  while (a <= n) {
    y <- y + M[a, b]
    a <- a + 1
    b <- b -1
  }
  z <- x + y - M[q, q]
  return(z)
}

east <- 1
south <- 0
west <- 0
north <- 0
i <- (ceiling(n / 2))
j <- (ceiling(n / 2))
l <- 1
m <- 1
grid <- matrix(NA, nrow = n, ncol = n, byrow = TRUE)

grid[i, j] <- 1
k <- 2
while (k <= (n ^ 2)) {
  while (l <= m) {
    if (east == 1) {
      grid[i, (j + 1)] <- k
      j <- j + 1
      k <- k + 1
      l <- l + 1
    } else if (south == 1) {
      grid[(i + 1), j] <- k
      i <- i + 1
      k <- k + 1
      l <- l + 1
    } else if (west == 1) {
      grid[i, (j - 1)] <- k
      j <- j - 1
      k <- k + 1
      l <- l + 1
    } else if (north == 1) {
      grid[(i - 1), j] <- k
      i <- i - 1
      k <- k + 1
      l <- l + 1
    }
  }
  if (east == 1) {
    east <- 0
    south <- 1
    l <- 1
  } else if (south == 1) {
    south <- 0
    west <- 1
    m <- m + 1
    l <- 1
  } else if (west == 1) {
    west <- 0
    north <- 1
    l <- 1
  } else if (north == 1) {
    north <- 0
    east <- 1
    m <- m + 1
    l <- 1
  }
}

z <- diag(n, grid)


#print(grid)
print(z)
end_time <- Sys.time()
process_time <- end_time - beg_time
print(process_time)