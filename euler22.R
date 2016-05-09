library(dplyr)


name <- read.delim("p022_names.txt", header = FALSE, sep = ",", stringsAsFactors = FALSE, na.strings = "na")

name <- name[order(name)]
score <- 0

y <- function(x) {
  if (x == "A") {
    score <- 1
  } else if (x == "B") {
    score <- 2
  } else if (x == "C") {
    score <- 3
  } else if (x == "D") {
    score <- 4
  } else if (x == "E") {
    score <- 5
  } else if (x == "F") {
    score <- 6
  } else if (x == "G") {
    score <- 7
  } else if (x == "H") {
    score <- 8
  } else if (x == "I") {
    score <- 9
  } else if (x == "J") {
    score <- 10
  } else if (x == "K") {
    score <- 11
  } else if (x == "L") {
    score <- 12
  } else if (x == "M") {
    score <- 13
  } else if (x == "N") {
    score <- 14
  } else if (x == "O") {
    score <- 15
  } else if (x == "P") {
    score <- 16
  } else if (x == "Q") {
    score <- 17
  } else if (x == "R") {
    score <- 18
  } else if (x == "S") {
    score <- 19
  } else if (x == "T") {
    score <- 20
  } else if (x == "U") {
    score <- 21
  } else if (x == "V") {
    score <- 22
  } else if (x == "W") {
    score <- 23
  } else if (x == "X") {
    score <- 24
  } else if (x == "Y") {
    score <- 25
  } else if (x == "Z") {
    score <- 26
  }
  return(score)
}

i <- 1
j <- 1
k <- 0
l <- 0
m <- c()

while (i <= length(name)) {
  while (j <= nchar(name[i])) {
    l <- substr(name[i], j, j)
    z <- y(l)
    #print(z)
    #print(l)
    k <- k + z 
    j <- j + 1
  }
  m <- append(m, k*i)
  j <- 1
  k <- 0
  l <- 0
  i <- i + 1
}

print(m)
print(sum(m))