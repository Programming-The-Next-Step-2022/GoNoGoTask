#' Check the reaction times in your data
#' 
#' The \emph{check_rt} function helps spot unusual patterns in your
#' reaction time data by pointing out unusually long distances between
#' sorted observations.
#'
#' @param data The output from gonogo()
#' @param ratio The ratio of the distance between any two (sorted) 
#'              data points. For example, the default 1/3 means that
#'              the user is notified when there is a distance between 
#'              any two sorted data points that is greater than one 
#'              third of the range of the data.
#'
#' @return Returns tips for processing the reaction times.
#' @export
#' 
#' @details For more information about how to handle reaction time data,
#'          please see \href{https://doi.org/10.3389/fpsyg.2021.675558}{article}.
#'
#' @examples
#' check_rt(data, ratio = 1/4)
check_rt <- function(data, ratio = 1/3) {
   
   # Create a threshold based on the ratio
   threshold <- ratio * abs(range(data$rt, na.rm = TRUE)[1] - 
                            range(data$rt, na.rm = TRUE)[2])
  
  # Create a vector of the differences between (sorted) reaction times
  dist <- c()
  for (i in 1:(sum(!is.na(data$rt))-1)) {
    dist[i] <- abs(sort(data$rt)[i+1] - sort(data$rt)[i])
  }
  
  # Notify of possible gaps and small min values in the dat
  if (sum(dist >= threshold) == 1) {
    print(paste0("There are observations in your reaction times that are unusually far from each other: the ratio of the distance to the range is greater than ", round(ratio, 2),". This is problematic as it means that your dat form two clusters, one of which likely consists of unrealistically small reaction times. These are typical in Go-No Go dat as responses that are too slow tend to leak into the next trial and appear as extremely small reaction times on that trial. A potential threshold to consider for removing unusually short reaction times is ", round(threshold, 3), ". You can take a closer look at the distribution of the reaction times here:"))
    hist(data$rt, breaks = 20, col = "lightblue", 
         main = "Reaction Times", xlab = NULL)
    
  } else if (sum(dist >= threshold) > 1) {
    print(paste0("There are multiple gaps in your reaction time dat, meaning that they form more than two clusters that are further than ", threshold, " from each other. Take a look at the distribution of your dat below:"))
    hist(data$rt, breaks = 20, col = "lightblue", 
         main = "Reaction Times", xlab = NULL)
    
  } else {
    print(paste0("There are no gaps in your reaction time data according to the ", ratio, " you set."))
  }
}