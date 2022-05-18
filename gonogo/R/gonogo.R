# The Go-No Go Task

# ---------------
# Load libraries
# ---------------

#library(magick)
library(shiny)

# -----------------
# Create functions
# -----------------

# INPUT (probably): interval, n_trial, pin/id/person
interval <- 0.5
n_trial <- 20
id <- "Milla"

# Function to draw the screen
drawScreen <- function(txt, cex=1, col="black") {
  plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="")
  text(x=0.5, y=0.5, labels=txt, cex=cex)
}

# Create stimuli
stim <- sample(x = c("A", "X"), replace = TRUE, size = n_trial)

# Create stimuli (random)
stim <- sample(x = c("A", "X"), replace = TRUE, size = n_trial)

# Create dataset
data <- data.frame(id = factor(rep(id, n_trial), levels = c(id)),
                   response = factor(rep("", n_trial), levels = c(" ", "none")),
                   correct = rep(100, n_trial),
                   SDT = factor(rep("", n_trial), levels = c("hit", "miss", "falsealarm", "correctrejection")),
                   rt = rep(100, n_trial),
                   stimulus = factor(stim, levels = c("A", "X")))

# Prints the response (spacebar press) and time it took to press it
# Two problems: the time resets every time you press any key besides spacebar,
# and function does not stop until it is pressed
rrt <- function(choiceKeys=c(" ")) {

  dynamic_readline <- function() {
    while (rstudioapi::isAvailable()) {
      input <- rstudioapi::getConsoleEditorContext()$contents

      if (input != "") {
        rstudioapi::sendToConsole("", execute = FALSE)
        return(input)
      }
    }
    readline()
  }

  repeat {
    rt <- system.time({
      choice <- dynamic_readline()
    })[3]
    if (choice %in% choiceKeys) break
  }
  setNames(c(choice, rt), c("response", "rt"))
}








# IGNORE: screens?

# Screen 1
plot(NA, NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="")
text(x=0.5, y=0.5, labels="Welcome to the Go-No Go Task!", cex=1.5)

# Screen 2: separate function for this?
plot(NA, NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="")
text(x=0.5, y=0.5, labels="Instructions: In this task, you will see two types of stimuli, the letter A and the\nletter X. When you see the letter A, you should respond as quickly as\npossible by pressing the space bar. When you see the letter X, you should\nnot respond (do not press any key). You should try to be as fast and accurate\nas possible.",
     cex=0.8)

# Screen 3
plot(NA, NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="")
text(x=0.5, y=0.5, labels="Press enter to start the task.", cex=1.5)

# Screen 4
plot(NA, NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="")
text(x=0.5, y=0.5, labels="A", cex=3)

# Screen 5
plot(NA, NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="")
text(x=0.5, y=0.5, labels="X", cex=3)

# Screen 6
plot(NA, NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="")
text(x=0.5, y=0.5, labels="You finished the task. Thank you for participating!", cex=1)





























# SIMULATION?

# -------------
# Load stimuli
# -------------

#A <- list(img = image_read("https://www.iconsdb.com/icons/download/blue/circle-256.jpg"),
#                name = "A")
# print(A)

#X <- list(img = image_read("https://www.iconsdb.com/icons/download/red/circle-256.jpg"),
#                 name = "X")
# print(X)

#stimuli <- list(go = A, nogo = X) # not sure if this is the best format

# -------------------------------------------------
# Create a while loop that is not interactive (yet)
# -------------------------------------------------

# Create an empty file that records ALL answers?
# Create an empty file that records answer given that round?

# Prespecify parameters
rounds <- 10
time <- .5 # not useful yet but we need to set the interval between rounds if no response is given
stimuli <- c("A", "X")
stimulus <- sample(x = stimuli, size = 1)
response <- sample(x = c("response", "noresponse"), size = 1)
rt <- sample(x = c(seq(from = 0, to = .5, by = .005), rep(1,100)), size = 1)
output_example <- data.frame(correct = rep(x = 0, times = rounds),
                             SDT = rep(x = 0, times = rounds),
                             rt = rep(x = 0, times = rounds),
                             stimulus = rep(x = 0, times = rounds))

for (round in 1:rounds) {

  # Randomly assign stimulus shown for that round
  # stimulus <- sample(x = stimuli, size = 1)

  # Randomly assign response (THIS HAS TO BE PARTICIPANT'S RESPONSE / interactive)
  # response <- sample(x = c("response", "noresponse"), size = 1)
  # Randomly assign reaction time (THIS HAS TO BE PARTICIPANT'S RESPONSE TIME / interactive)
  # rt <- sample(x = c(seq(from = 0, to = .5, by = .005), rep(1,100)), size = 1)
  # rt = 1 here signifies no reaction; not sure if the response variable is necessary?

  if (stimulus == "A" && rt <= .5) {

    # Store output
    output_example$correct[round] <- 1
    output_example$SDT[round] <- "hit"
    output_example$rt[round] <- rt
    output_example$stimulus[round] <- "go"

    # Assign new round (not necessary when I figure out the actual interactive bit)
    stimulus <- sample(x = stimuli, size = 1)
    rt <- sample(x = c(seq(from = 0, to = .5, by = .005), rep(1,100)), size = 1)
    # round <- round + 1

  } else if (stimulus == "A" && rt > .5) {

    # Store output
    output_example$correct[round] <- 0
    output_example$SDT[round] <- "miss"
    output_example$rt[round] <- rt
    output_example$stimulus[round] <- "go"

    # Assign new round (not necessary when I figure out the actual interactive bit)
    stimulus <- sample(x = stimuli, size = 1)
    rt <- sample(x = c(seq(from = 0, to = .5, by = .005), rep(1,100)), size = 1)
    # round <- round + 1

  } else if (stimulus == "X" && rt <= .5) {

    # Store output
    output_example$correct[round] <- 0
    output_example$SDT[round] <- "falsealarm"
    output_example$rt[round] <- rt
    output_example$stimulus[round] <- "nogo"

    # Assign new round (not necessary when I figure out the actual interactive bit)
    stimulus <- sample(x = stimuli, size = 1)
    rt <- sample(x = c(seq(from = 0, to = .5, by = .005), rep(1,100)), size = 1)
    # round <- round + 1

  } else if (stimulus == "X" && rt > .5) {

    # Store output
    output_example$correct[round] <- 1
    output_example$SDT[round] <- "correctrejection"
    output_example$rt[round] <- rt
    output_example$stimulus[round] <- "nogo"

    # Assign new round (not necessary when I figure out the actual interactive bit)
    stimulus <- sample(x = stimuli, size = 1)
    rt <- sample(x = c(seq(from = 0, to = .5, by = .005), rep(1,100)), size = 1)
    # round <- round + 1

  }

  # End of the loop
  # print(output_example)

}

print(output_example)






