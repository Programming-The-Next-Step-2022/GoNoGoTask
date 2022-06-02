
#' A Go-No Go Task in the R console
#'
#' @param id Either a character string specifying the participant's name, or a numeric vector of length 1 specifying the participant's id number
#' @param n_trial Number of trials (numeric vector of length 1)
#' @param stimuli A character vector of length 2: first element specifies the 'go' element, second stimulus specifies the 'no go' stimuls (default is c("A", "X"))
#' @param inter Length of each trial in seconds (numeric vector of length 1) (default is 0.6)
#' @param prb A numeric vector of length 2 specifying the probability weights for the stimuli (default is c(0.5, 0.5))
#'
#' @return A dataframe consisting of \emph{n_trial} rows and six columns: \emph{id} (participant's name or numeric id),  \emph{response} (response key used on the trial),  \emph{correct} (1=correct, 0=incorrect),  \emph{SDT} (responses categorised according to Signal Detection Theory),  \emph{rt} (reaction time), and  \emph{stimulus} (the stimulus shown on the trial)
#' @export
#'
#' @examples
#' p1_data <- gonogo(id = "p1", n_trial = 50, stimuli = c("T", "S"), inter = 0.6, prb = c(0.7, 0.3))
gonogo <- function(id, n_trial, stimuli=c("A", "X"), inter=0.6,
                   prb = c(0.75, 0.25)) {

  # Check class and length of arguments
  if (!(class(id) %in% c("character", "numeric")) || length(id) != 1) {
    stop("id must be either a numeric vector of length 1 (pin number identifying the participant), or a character vector of length 1 (name of participant)")
  }
  if (class(n_trial) != "numeric" || length(n_trial) != 1) {
    stop("n_trial must be a numeric vector of length 1")
  }
  if (class(stimuli) != "character" || length(stimuli) != 2) {
    stop("stimuli must be a character vector of length 2")
  }
  if (class(inter) != "numeric" || length(inter) != 1) {
    stop("inter must be a numeric vector of length 1")
  }
  if (class(prb) != "numeric" || length(prb) != 2 || prb[1] + prb[2] != 1) {
    stop("prb must be a numeric vector of length 2, and its components must add up to exactly 1")
  }

  # Create the randomized set of stimuli
  stim <- sample(x = stimuli, size = n_trial, prob = prb, replace = TRUE)

  # Create the (output) dataset
  data <- data.frame(id = factor(rep(id, n_trial), levels = c(id)),
                     response = factor(rep("none", n_trial), levels = c(" ", "none", "space")),
                     correct = rep(2, n_trial),
                     SDT = factor(rep("hit", n_trial), levels = c("hit", "miss", "falsealarm", "correctrejection")),
                     rt = rep(100, n_trial),
                     stimulus = stim)

  # Intro screens
  drawScreen("Welcome to the Go-No Go Task!\nPress Enter to continue.", cex=1.5)
  readline()
  drawScreen(paste("Instructions: In this task, you will see two types of stimuli,", stimuli[1], "and\n", stimuli[2], ". When you see", stimuli[1], "you should respond as quickly as\npossible by pressing the space bar. When you see", stimuli[2], "you should\nnot respond (do not press any key). You should try to be as fast and accurate\nas possible. Press Enter to continue."),
             cex=0.8)
  readline()
  drawScreen("Press Enter when you are ready to start the task.", cex=1)
  readline()
  drawScreen("Countdown: 3"); Sys.sleep(1)
  drawScreen("Countdown: 2"); Sys.sleep(1)
  drawScreen("Countdown: 1"); Sys.sleep(1)
  drawScreen(""); Sys.sleep(0.5)

  # The for loop
  for (i in 1:n_trial) {
    drawScreen(stim[i], 3)
    data[i, c("response", "rt")] <- rrt(choiceKeys = c(" "), interval = inter)
    drawScreen(""); Sys.sleep(0.3)
  }

  # End screen
  drawScreen("You finished the task. Thank you for participating!", cex=1)

  # Update the dataset
  for (i in 1:nrow(data)) {
    if (data[i, "stimulus"] == stimuli[1] && data[i, "response"] == " ") {
      data[i, "correct"] <- 1
      data[i, "SDT"] <- "hit"
    } else if (data[i, "stimulus"] == stimuli[2] && data[i, "response"] == " ") {
      data[i, "correct"] <- 0
      data[i, "SDT"] <- "falsealarm"
    } else if (data[i, "stimulus"] == stimuli[1] && data[i, "response"] != " ") {
      data[i, "correct"] <- 0
      data[i, "SDT"] <- "miss"
    } else {
      data[i, "correct"] <- 1
      data[i, "SDT"] <- "correctrejection"
    }
  }

  data$rt <- as.numeric(data$rt)
  for (i in 1:nrow(data)) {
    if (data$rt[i] == 100) {
      data$rt[i] <- NA
    }
  }

  for (i in 1:nrow(data)) {
    if (data$response[i] == " ") {
      data$response[i] <- "space"
    }
  }

  print(data)
}


# Ideas?
# different stimuli: photos, characters, numbers / multiple stimuli?
# can you break up the function into smaller bits?
# why does rrt() give reaction times higher than the specified interval?
