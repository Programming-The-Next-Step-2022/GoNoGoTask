#' Update the Go-No Go data within the function
#' 
#' The \emph{update_data} function is a helper function for the 
#' \emph{play_gonogo} function. It takes the raw data and transforms
#' it into a more user-friendly format.
#'
#' @param data The dataframe within the \emph{play_gonogo} function in
#'             the unedited format
#' 
#' @param stimuli The Go stimulus and No Go stimulus in a character vector
#'                of length 2, as specified in the play_gonogo() function
#' 
#' @param mean_error The mean error (average time it takes to run the 
#'                   \emph{rrt}) function). The default is 0.0334282, but
#'                   this might vary between devices. See Details for more
#'                   information.
#'
#' @return The updated dataframe with six columns: \emph{n_trial*n_block} 
#'         rows and seven columns: \emph{id} (participant's name or 
#'         numeric id),  \emph{response} (response key used on the trial),
#'         \emph{correct} (1=correct, 0=incorrect),  \emph{SDT} (responses 
#'         categorized according to Signal Detection Theory),  \emph{rt} 
#'         (reaction time in seconds),  \emph{stimulus} (the stimulus shown
#'         on the trial), and \emph{block} (the number of the block)
#'
#' @details It should be noted that, for optimal accuracy, the mean error
#'          (average time it takes to run the \emph{rrt} function) should
#'          be calculated for each device used as it might vary.
#'
#' @examples
#' update_data(data = data, stimuli = stimuli)
update_data <- function(data, stimuli = stimuli, mean_error = 0.0334282) {
  
  # Replace " " with "space"
  for (i in 1:nrow(data)) {
    if (data$response[i] == " ") {
      data$response[i] <- "space"
    }
  }
  
  # Subtract the mean error (time it takes to run the function rrt()) 
  # from the reaction times for increased accuracy
  data$rt <- as.numeric(data$rt)
  for (i in 1:nrow(data)) {
    if (!is.na(data$rt[i])) { # think about the second condition
      data[i, "rt"] <- data[i, "rt"] - mean_error
    }
  }
  
  # Update the correct and SDT columns
  for (i in 1:nrow(data)) {
    if (data[i, "stimulus"] == stimuli[1] &&
        data[i, "response"] == "space") {
      data[i, "correct"] <- 1
      data[i, "SDT"] <- "hit"
    } else if (data[i, "stimulus"] == stimuli[2] &&
               data[i, "response"] == "space") {
      data[i, "correct"] <- 0
      data[i, "SDT"] <- "falsealarm"
    } else if (data[i, "stimulus"] == stimuli[1] &&
               data[i, "response"] != "space") {
      data[i, "correct"] <- 0
      data[i, "SDT"] <- "miss"
    } else {
      data[i, "correct"] <- 1
      data[i, "SDT"] <- "correctrejection"
    }
  }
  return(data)
}

