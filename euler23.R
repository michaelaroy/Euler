# Project Euler Problem 23:
#1. Find all abundant numbers (vector) - V1
#- numbers whose factors are greater than the number
#2. Find sum of all combinations of any two numbers (vector) - V2
#- include itself in combinations
#3. For all positive integers up to 28123: if i is not in V2, append to vector - V3
#4. Sum V3

library(dplyr)

beg_time <- Sys.time()
n <- 28123

y <- function(x) {
  c <- c()
  i <- 1
  while (i < sqrt(x)) {
    if ((x %% i) == 0) {
      c <- append(c, i)
      if (i > 1) {
        c <- append(c, (x / i))
        i <- i + 1
      } else if ((sqrt(x) %% 1 == 0)) {
        c <- append(c, sqrt(x))
        i <- i + 1
      } else {
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }
  z <- sum(c)
  return(z)
}

#abundant_sums <- data.frame()
                            

sum_factors <- c()

j <- 1
numbers <- c()
while (j < n) {
  sum_factors <- append(sum_factors, y(j))
  #numbers <- append(numbers, j)
  #abundant_sums <- rbind(abundant_sums, j)
  j <- j + 1
}


#colnames(abundant_sums) <- c("Numbers")
#abundant_sums <- abundant_sums %>% mutate(SumFactors = sum_factors) %>% filter(SumFactors > Numbers)

#print(sum_factors)

k <- 1
l <- 1
abundant <- c()
v <- c()
w <- c()

while (k <= length(sum_factors)) {
  if (sum_factors[k] > k) {
    abundant <- append(abundant, k)
  }
  
  while (l <= length(abundant)) {
    m <- k - abundant[l]
    if (is.element(m, abundant)) {
      v <- append(v, k)
      break
    #} else if (l == length(abundant)) {
    #  w <- append(w, k)
    #  break
    } else {
      l <- l + 1
    }
  }
  
  print(max(v))
  l <- 1
  k <- k + 1
}

#sum_sums <- data.frame(NumberSum = integer(), row.names = NULL, colnames("NumberSum"))
#colnames(sum_sums) <- c("NumberSum")
#sum_sums$NumberSum[1] <- rbind(sum_sums, abundant_sums$Numbers[1])
#sum_sums %>% mutate(abundant_sums$Numbers)
#print(abundant)
#t <- min(which(abundant > (n / 2)))
#t <- 50


"m <- 1

count_i <- 0
count_vec <- c()




while (l < t) {
  count_i <- 0
  print(l)
  r <- n - abundant[l]
  s <- min(which(abundant > r))
  print(s)
  while (m < s) {
    p <- abundant[l] + abundant[m]
    if (is.element(p, v)) {
      m <- m + 1
    } else if (p > n) {
      break
    } else {
      print(m)
      v <- append(v, p)
      count_i <- count_i + 1
      m <- m + 1
    }
  }
  count_vec <- append(count_vec, count_i)
  m <- l + 1
  l <- l + 1
}"



#v <- sort(unique(v))
#print(v)

q <- 1
w <- c()
while (q <= length(v)) {
  if (is.element(q, v)) {
    q <- q + 1
  } else {
    w <- append(w, q)
    q <- q + 1
  }
}

#print(w)
nonabundant_sum <- sum(w)
print(nonabundant_sum)
end_time <- Sys.time()
process_time <- end_time - beg_time
print(process_time)