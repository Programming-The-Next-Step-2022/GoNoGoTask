# Update the dataset
for (rows in 1:nrow(data)) {
  if (data[rows, "stimulus"] == stimuli[1] && data[rows, "response"] == " ") {
    data[rows, "correct"] <- 1
    data[rows, "SDT"] <- "hit"
  } else if (data[rows, "stimulus"] == stimuli[2] && data[rows, "response"] == " ") {
    data[rows, "correct"] <- 0
    data[rows, "SDT"] <- "falsealarm"
  } else if (data[rows, "stimulus"] == stimuli[1] && data[rows, "response"] != " ") {
    data[rows, "correct"] <- 0
    data[rows, "SDT"] <- "miss"
  } else {
    data[rows, "correct"] <- 1
    data[rows, "SDT"] <- "correctrejection"
  }
}

data$rt <- as.numeric(data$rt)
for (rows in 1:nrow(data)) {
  if (data$rt[rows] == 100) {
    data$rt[rows] <- NA
  }
}

for (rows in 1:nrow(data)) {
  if (data$response[rows] == " ") {
    data$response[rows] <- "space"
  }
}

for (rows in 1:nrow(data)) {
  if (!is.na(data$rt[rows]) && data$rt[rows] > mean_error) { # think about the second condition
    data[rows, "rt"] <- data[rows, "rt"]-mean_error
  }
}
