library(dplyr)


a1 <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
a2 <- c("eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
a3 <- c("ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
a4 <- c("hundred", "and")
a5 <- c("one", "thousand")

num_words <- c()

n <- 1000
i <- 1
j <- 1
k <- 1
l <- 1

while (i <= n) {
  j <- floor(i / 10)
  k <- (i %% 10)
  l <- (i %% 100)
  m <- floor(i / 100)
  q <- floor(l / 10)
  if (i < 10) {
    num_words <- append(num_words, a1[i])
    i <- i + 1
  } else if (i == 10) {
    num_words <- append(num_words, a3[j])
    i <- i + 1
  } else if (i < 20) {
    num_words <- append(num_words, a2[k])
    i <- i + 1
  } else if ((i < 99) & (k == 0)) {
    num_words <- append(num_words, a3[j])
    i <- i + 1
  } else if (i < 100) {
    num_words <- append(num_words, paste(a3[j], a1[k]))
    i <- i + 1
  } else if ((i < 1000) & (l == 0)) {
    num_words <- append(num_words, paste(a1[m], a4[1]))
    i <- i + 1
  } else if ((i < 1000) & (l < 10)) {
    num_words <- append(num_words, paste(a1[m], a4[1], a4[2], a1[l]))
    i <- i + 1
  } else if ((i < 1000) & (l == 10)) {
    num_words <- append(num_words, paste(a1[m], a4[1], a4[2], a3[q]))
    i <- i + 1
  } else if ((i < 1000) & (l < 20)) {
    num_words <- append(num_words, paste(a1[m], a4[1], a4[2], a2[k]))
    i <- i + 1
  } else if ((i < 1000) & (l < 100)) {
    num_words <- append(num_words, paste(a1[m], a4[1], a4[2], a3[q], a1[k]))
    i <- i + 1
  } else {
    num_words <- append(num_words, paste(a5[1], a5[2]))
    i <- i + 1
  }
}

print(num_words)
sum(nchar(gsub(" ", "", num_words)))
