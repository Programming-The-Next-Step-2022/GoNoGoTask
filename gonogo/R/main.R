#' The Go-No Go Task
#'
#' @param id Either a character vector of length 1 specifying the participant's name, or a numeric vector of length 1 specifying a unique id number
#' @param n_trial A numeric vector of length 1 specifying the number of trials per block (default is 40)
#' @param n_block A numeric vector of length 1 specifying the number of blocks (default is 4)
#' @param stimuli A character vector of length 2 specifying the Go stimulus and the No Go stimulus (in that order; default is A and X)
#' @param inter A numeric vector of length 1 specifying the length of each trial in seconds (default is 0.6)
#' @param prb A numeric vector of length 2 specifying the probability weights for the Go stimulus and No Go stimulus (in that order; default is 0.75 and 0.25), must add up to 1
#'
#' @return A dataframe consisting of \emph{n_trial*n_block} rows and seven columns: \emph{id} (participant's name or numeric id),  \emph{response} (response key used on the trial),  \emph{correct} (1=correct, 0=incorrect),  \emph{SDT} (responses categorized according to Signal Detection Theory),  \emph{rt} (reaction time in seconds),  \emph{stimulus} (the stimulus shown on the trial), and \emph{block} (the number of the block)
#' @export
#'
#' @examples
#' data_p1 <- play_gonogo(id = "p1", n_trial = 40, n_block = 4)
play_gonogo <- function(id, n_trial = 40, n_block = 4, stimuli = c("A", "X"), 
                   inter = 0.6, prb = c(0.75, 0.25)) {
  
  # Check class and length of arguments
  if (!(class(id) %in% c("character", "numeric")) || length(id) != 1) {
    stop("id must be either a numeric vector of length 1 (pin number identifying the participant), or a character vector of length 1 (name of participant)")
  }
  if (class(n_trial) != "numeric" || length(n_trial) != 1) {
    stop("n_trial must be a numeric vector of length 1")
  }
  if (class(n_block) != "numeric" || length(n_block) != 1) {
    stop("n_block must be a numeric vector of length 1")
  }
  if (class(stimuli) != "character" || length(stimuli) != 2) {
    stop("stimuli must be a character vector of length 2")
  }
  if (class(inter) != "numeric" || length(inter) != 1) {
    stop("inter must be a numeric vector of length 1")
  }
  if (class(prb) != "numeric" || length(prb) != 2 || prb[1] + prb[2] != 1) {
    stop("prb must be a numeric vector of length 2, and its components must add up to 1")
  }
  
  # Create practice stimuli
  practice <- sample(x = stimuli, size = 10, replace = TRUE)
  
  # Create separate dataframes for each block
  all_data <- list()
  for (j in 1:n_block) {
    all_data[[j]] <- data.frame(id = factor(rep(id, n_trial), 
                                            levels = c(id)),
                                response = factor(rep("none", n_trial), 
                                                  levels = c(" ", 
                                                             "none", 
                                                             "space")),
                                correct = rep(2, n_trial),
                                SDT = factor(rep("hit", n_trial), 
                                             levels = c("hit", 
                                                        "miss", 
                                                        "falsealarm", 
                                                        "correctrejection")),
                                rt = rep(100, n_trial),
                                stimulus = sample(x = stimuli, 
                                                  size = n_trial, 
                                                  prob = prb, 
                                                  replace = TRUE),
                                block = rep(j, n_trial)
    )
  }
  
  # Intro screens
  draw_screen(paste("Welcome to the Go-No Go Task", id,"!\nPress Enter to continue."), cex = 1.3)
  readline()
  draw_screen(paste("Instructions \nIn this task, you will see two types of stimuli,", stimuli[1], "and\n", stimuli[2], ". When you see", stimuli[1], "you should respond as quickly as\npossible by pressing the space bar. When you see", stimuli[2], "you should\nnot respond (do not press any key). You should try to be as fast and accurate\nas possible. Press Enter to continue."),
             cex = 0.75)
  readline()
  draw_screen(paste("The task consists of", n_block, "blocks, each consisting of", n_trial, "trials.\nThere is is a break between each pair of blocks.\nBefore starting the task, there is one practice block of 10 trials.\n Press Enter to start the practice trials."),
             cex = 0.75)
  readline()
  draw_screen("")
  Sys.sleep(0.5)
  
  # Practice trials
  draw_screen("Practice trials")
  Sys.sleep(1)
  draw_screen("Countdown: 3")
  Sys.sleep(1)
  draw_screen("Countdown: 2")
  Sys.sleep(1)
  draw_screen("Countdown: 1")
  Sys.sleep(1)
  draw_screen("")
  Sys.sleep(0.5)
  for (p in 1:10) {
    draw_screen("")
    Sys.sleep(0.1)
    draw_screen("+", 3)
    Sys.sleep(0.4)
    draw_screen("")
    Sys.sleep(0.1)
    draw_screen(practice[p], 3)
    rrt(choiceKeys = c(" "), interval = inter)
  }
  draw_screen("End of practice trials.\nPress Enter to continue on to the task.",
             cex = 1)
  readline()
  
  # The for loops
  for (j in 1:n_block) {
    draw_screen(paste("Block", j))
    Sys.sleep(1)
    draw_screen("Countdown: 3")
    Sys.sleep(1)
    draw_screen("Countdown: 2")
    Sys.sleep(1)
    draw_screen("Countdown: 1")
    Sys.sleep(1)
    draw_screen("")
    Sys.sleep(0.5)
    for (i in 1:n_trial) {
      draw_screen("")
      Sys.sleep(0.1)
      draw_screen("+", 3)
      Sys.sleep(0.4)
      draw_screen("")
      Sys.sleep(0.1)
      draw_screen(all_data[[j]][i, "stimulus"], 3)
      all_data[[j]][i, c("response", "rt")] <- rrt(choiceKeys = c(" "), 
                                                  interval = inter)
    }
    draw_screen(paste("End of Block", j, "!\n Press Enter to continue."),
               cex = 1)
    readline()
  }
  
  # End screen
  draw_screen(paste("You finished the task. Thank you for participating,",id,"!"), cex = 1)
  
  # Bind the datasets
  for (k in 1:(length(all_data)-1)) {
    all_data[[k+1]] <- rbind(all_data[[k]], all_data[[k+1]])
  }
  data <- all_data[[length(all_data)]]

  # Update the dataset
  data <- update_data(data, stimuli = stimuli)
  print(data)
}
