#' Update the Go-No Go data within the function
#'
#' @param data The dataframe in the unedited format
#' @param stimuli The Go stimulus and No Go stimulus in a character vector of length 2, as specified in the gonogo() function
#'
#' @return The updated dataframe with six columns: \emph{n_trial*n_block} rows and seven columns: \emph{id} (participant's name or numeric id),  \emph{response} (response key used on the trial),  \emph{correct} (1=correct, 0=incorrect),  \emph{SDT} (responses categorized according to Signal Detection Theory),  \emph{rt} (reaction time in seconds),  \emph{stimulus} (the stimulus shown on the trial), and \emph{block} (the number of the block)
#'
#' @examples
update_data <- function(data, stimuli = stimuli) {
  
  # Replace " " with "space"
  for (i in 1:nrow(data)) {
    if (data$response[i] == " ") {
      data$response[i] <- "space"
    }
  }
  
  # Subtract the mean error (time it takes to run the function rrt() from a
  # simulation of 1000 trials) from the reaction times for increased accuracy
  mean_error <- 0.02742448
  data$rt <- as.numeric(data$rt)
  for (i in 1:nrow(data)) {
    if (!is.na(data$rt[i])) { # think about the second condition
      data[i, "rt"] <- data[i, "rt"]-mean_error
    }
  }
  
  # Update the correct and SDT columns
  for (i in 1:nrow(data)) {
    if (data[i, "stimulus"] == stimuli[1] && data[i, "response"] == "space") {
      data[i, "correct"] <- 1
      data[i, "SDT"] <- "hit"
    } else if (data[i, "stimulus"] == stimuli[2] && data[i, "response"] == "space") {
      data[i, "correct"] <- 0
      data[i, "SDT"] <- "falsealarm"
    } else if (data[i, "stimulus"] == stimuli[1] && data[i, "response"] != "space") {
      data[i, "correct"] <- 0
      data[i, "SDT"] <- "miss"
    } else {
      data[i, "correct"] <- 1
      data[i, "SDT"] <- "correctrejection"
    }
  }
  return(data)
}

