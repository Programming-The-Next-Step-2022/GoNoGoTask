check_rt <- function(data, threshold = 1/3) {
  
  # Check for a "gap" between sorted reaction times
  
  
}

sort(data$rt)
sum(!is.na(data$rt))

abs(range(data$rt, na.rm = TRUE)[1] - range(data$rt, na.rm = TRUE)[2])
threshold <- 1/3 * abs(range(data$rt, na.rm = TRUE)[1] - range(data$rt, na.rm = TRUE)[2])


dist <- c()
for (i in 1:(sum(!is.na(data$rt))-1)) {
  dist[i] <- abs(sort(data$rt)[i+1] - sort(data$rt)[i])
}

sum(dist >= threshold)
