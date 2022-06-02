#' Check reaction times
#'
#' @param data The output from gonogo()
#' @param ratio The ratio of the distance between any two (sorted) data points. For example, the default 1/3 means that the user is notified when there is a distance between any two sorted data points that is greater than one third of the range of the data.
#'
#' @return Returns tips for processing the reaction times
#' @export
#'
#' @examples
#' function(data, ratio = 1/4)
check_rt <- function(data, ratio = 1/3) {
   
   # Create a threshold based on the ratio
   threshold <- ratio * abs(range(data$rt, na.rm = TRUE)[1] - 
                            range(data$rt, na.rm = TRUE)[2])
  
  # Create a vector of the differences between (sorted) reaction times
  dist <- c()
  for (i in 1:(sum(!is.na(data$rt))-1)) {
    dist[i] <- abs(sort(data$rt)[i+1] - sort(data$rt)[i])
  }
  
  # Notify of possible gaps in the data
  if (sum(dist >= threshold) > 0) {
    print(paste("There are observations in your reaction times that are unusually far from each other: the ratio of the distance to the range is greater than", ratio,". A potential threshold to consider for removing unusually short reaction times is", round(threshold, 3), ". You can take a closer look at the distribution of the reaction times below:"))
    hist(data$rt, breaks = 20, col = "lightblue", 
         main = "Reaction Times", xlab = NULL)
  } else {
    print(paste("There are no observations in your reaction times that are unsually far from each other according to the ratio provided."))
  }
  
}

