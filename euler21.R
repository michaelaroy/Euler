library(dplyr)

n <- 10000
factors <- c()
ami <- c()
sum_ami <- c()

i <- 1
j <- 1

while (i <= n) {
  while (j < sqrt(i)) {
    #print(i)
    m <- (i / j)
    print(i)
    if ((i %% j) == 0) {
      factors <- append(factors, j)
      if (((i %% m) == 0) & (i != m)) {
        factors <- append(factors, m)
        j <- j + 1
      } else {
        j <- j + 1
      }
    } else {
      j <- j + 1
    }
  }
  ami <- append(ami, i)
  sum_ami <- append(sum_ami, sum(factors))
  factors <- c()
  i <- i + 1
  j <- 1
}
#print(sum_ami)

i <- 2
j <- 1
k <- c()

while (i <= length(ami)) {
  #print(i)
  w <- ami[i]
  x <- sum_ami[i]
  y <- sum_ami[x]
  #z <- sum_ami[y]
  if (x > length(ami)) {
    i <- i + 1
  } else if ((w == y) & (w != x)) {
    k <- append(k, i)
    i <- i + 1
  } else {
    i <- i + 1
  }
}

print(k)
print(sum(k))