library(dplyr)
library(bit)



r1 <- c(75, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2 <- c(95, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3 <- c(17, 47, 82, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4 <- c(18, 35, 87, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r5 <- c(20, 4, 82, 47, 65, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r6 <- c(19, 1, 23, 75, 3, 34, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r7 <- c(88, 2, 77, 73, 7, 63, 67, 0, 0, 0, 0, 0, 0, 0, 0)
r8 <- c(99, 65, 4, 28, 6, 16, 70, 92, 0, 0, 0, 0, 0, 0, 0)
r9 <- c(41, 41, 26, 56, 83, 40, 80, 70, 33, 0, 0, 0, 0, 0, 0)
r10 <- c(41, 48, 72, 33, 47, 32, 37, 16, 94, 29, 0, 0, 0, 0, 0)
r11 <- c(53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14, 0, 0, 0, 0)
r12 <- c(70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57, 0, 0, 0)
r13 <- c(91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48, 0, 0)
r14 <- c(63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31, 0)
r15 <- c(4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23)


path <- matrix(c(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15), nrow = 15, byrow = TRUE)

n <- 16384
k <- 0
l <- ceiling(log2(n))
index_bin <- c(rep(0, l))
path_sums <- c()

y <- function(x) {
  index_bin <- c(rep(0, l))
  if (floor((x / (2 ^ (l-1))) %% 2) == 1) {
    index_bin[1] <- 1
  } else {
    index_bin[1] <- 0
  }
  
  if (floor((x / (2 ^ (l-2))) %% 2) == 1) {
    index_bin[2] <- 1
  } else {
    index_bin[2] <- 0
  }
  
  if (floor((x / (2 ^ (l-3))) %% 2) == 1) {
    index_bin[3] <- 1
  } else {
    index_bin[3] <- 0
  }
  
  if (floor((x / (2 ^ (l-4))) %% 2) == 1) {
    index_bin[4] <- 1
  } else {
    index_bin[4] <- 0
  }
  
  if (floor((x / (2 ^ (l-5))) %% 2) == 1) {
    index_bin[5] <- 1
  } else {
    index_bin[5] <- 0
  }
  
  if (floor((x / (2 ^ (l-6))) %% 2) == 1) {
    index_bin[6] <- 1
  } else {
    index_bin[6] <- 0
  }
  
  if (floor((x / (2 ^ (l-7))) %% 2) == 1) {
    index_bin[7] <- 1
  } else {
    index_bin[7] <- 0
  }
  
  if (floor((x / (2 ^ (l-8))) %% 2) == 1) {
    index_bin[8] <- 1
  } else {
    index_bin[8] <- 0
  }

  if (floor((x / (2 ^ (l-9))) %% 2) == 1) {
    index_bin[9] <- 1
  } else {
    index_bin[9] <- 0
  }
  if (floor((x / (2 ^ (l-10))) %% 2) == 1) {
    index_bin[10] <- 1
  } else {
    index_bin[10] <- 0
  }
  if (floor((x / (2 ^ (l-11))) %% 2) == 1) {
    index_bin[11] <- 1
  } else {
    index_bin[11] <- 0
  }
  if (floor((x / (2 ^ (l-12))) %% 2) == 1) {
    index_bin[12] <- 1
  } else {
    index_bin[12] <- 0
  }
  if (floor((x / (2 ^ (l-13))) %% 2) == 1) {
    index_bin[13] <- 1
  } else {
    index_bin[13] <- 0
  }
  if (floor((x / (2 ^ (l-14))) %% 2) == 1) {
    index_bin[14] <- 1
  } else {
    index_bin[14] <- 0
  }

  return(index_bin)
  
}

#while (k < n) {
#  m <- paste(rev(as.integer(intToBits(as.integer(k))[1:14])), collapse="")
#  print(bin(i)[1:14])
#  print(m)
#  k <- k + 1
#}



while (k < n) {
  i <- 1
  j <- 1
  m <- path[1,1]
  print(k)
  z <- y(k)
  print(z)
  while (i <= length(z)) {
    if (z[i] == 0) {
      m <- m + path[(i+1), j]
      i <- i + 1
    } else {
      j <- j + 1
      m <- m + path[(i+1), j]
      i <- i + 1
    }
  }
  print(m)
  path_sums <- append(path_sums, m)
  k <- k + 1
}

print(path)
which.max(path_sums)
print(max(path_sums))