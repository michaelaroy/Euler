library(dplyr)
library(bit)

i <- 1
j <- 1
k <- 1
m <- 3


leap_year <- 0
week <- c(1, 2, 3, 4, 5, 6, 7)
month <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
year <- c(1901:2000)
date_vec <- c()
dates <- data_frame()

while (i <= length(year)) {
  print(year[i])
  if ((year[i] %% 100) == 0 & (year[i] %% 400) == 0) {
    leap_year <- 1
  } else if (year[i] %% 4 == 0) {
    leap_year <- 1
  } else {
    leap_year <- 0
  }

  while (j <= length(month)) {
    if (month[j] == 1 | month[j] == 3 | month[j] == 5 | month[j] == 7 | month[j] == 8 | month[j] == 10 | month[j] == 12) {
      day <- c(1:31)
    } else if (month[j] == 4 | month[j] == 6 | month[j] == 9 | month[j] == 11) {
      day <- c(1:30)
    } else if (month[j] == 2 & leap_year == 1) {
      day <- c(1:29)
    } else {
      day <- c(1:28)
    }
    while (k <= length(day)) {
      date_vec <- append(date_vec, j)
      date_vec <- append(date_vec, k)
      date_vec <- append(date_vec, year[i])
      date_vec <- append(date_vec, week[m])
      dates <- rbind(dates, date_vec)
      #print(dates)
      #print(paste(j, k, year[i], month[m]))
      if (m == 7) {
        m <- 1
      } else {
        m <- m + 1
      }
      date_vec <- c()
      k <- k + 1
    }
    k <- 1
    j <- j + 1
  }
  j <- 1
  i <- i + 1
}
colnames(dates) <- c("Month", "Day", "Year", "DayofWeek")