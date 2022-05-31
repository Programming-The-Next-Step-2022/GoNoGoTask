# Record time for the function to run so you can subtract it
rrt_mean_error <- function() {
  
  # Create vectors
  start_rrt <- c()
  finish_rrt <- c()
  diff_rrt <- c()
  
  # Simulate data from 1000 trials
  for (i in 1:1000) {
    start_rrt[i] <- Sys.time()
    rrt()
    finish_rrt[i] <- Sys.time()
    diff_rrt[i] <- (finish_rrt[i]-start_rrt[i])[[1]]
  }
  
  # Subtract known trial time to get the error for each trial
  diff_rrt <- diff_rrt-0.6 
  
  # Print the average error
  print(mean(diff_rrt))
  
}
