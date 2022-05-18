# The Go-No Go Task

# ---------------
# Load libraries
# ---------------

library(magick)
library(shiny)

# -------------
# Load stimuli
# -------------

go_blue <- list(img = image_read("https://www.iconsdb.com/icons/download/blue/circle-256.jpg"),
                name = "go_blue")
# print(go_blue)

nogo_red <- list(img = image_read("https://www.iconsdb.com/icons/download/red/circle-256.jpg"),
                 name = "nogo_red")
# print(nogo_red)

stimuli <- list(go = go_blue, nogo = nogo_red) # not sure if this is the best format

# -------------------------------------------------
# Create a while loop that is not interactive (yet)
# -------------------------------------------------

# Create an empty file that records ALL answers?
# Create an empty file that records answer given that round?

# Prespecify parameters
rounds <- 10
time <- .5 # not useful yet but we need to set the interval between rounds if no response is given
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

  if (stimulus[[1]][2] == "go_blue" && rt <= .5) {

    # Store output
    output_example$correct[round] <- 1
    output_example$SDT[round] <- "hit"
    output_example$rt[round] <- rt
    output_example$stimulus[round] <- "go"

    # Assign new round (not necessary when I figure out the actual interactive bit)
    stimulus <- sample(x = stimuli, size = 1)
    rt <- sample(x = c(seq(from = 0, to = .5, by = .005), rep(1,100)), size = 1)
    # round <- round + 1

  } else if (stimulus[[1]][2] == "go_blue" && rt > .5) {

    # Store output
    output_example$correct[round] <- 0
    output_example$SDT[round] <- "miss"
    output_example$rt[round] <- rt
    output_example$stimulus[round] <- "go"

    # Assign new round (not necessary when I figure out the actual interactive bit)
    stimulus <- sample(x = stimuli, size = 1)
    rt <- sample(x = c(seq(from = 0, to = .5, by = .005), rep(1,100)), size = 1)
    # round <- round + 1

  } else if (stimulus[[1]][2] == "nogo_red" && rt <= .5) {

    # Store output
    output_example$correct[round] <- 0
    output_example$SDT[round] <- "falsealarm"
    output_example$rt[round] <- rt
    output_example$stimulus[round] <- "nogo"

    # Assign new round (not necessary when I figure out the actual interactive bit)
    stimulus <- sample(x = stimuli, size = 1)
    rt <- sample(x = c(seq(from = 0, to = .5, by = .005), rep(1,100)), size = 1)
    # round <- round + 1

  } else if (stimulus[[1]][2] == "nogo_red" && rt > .5) {

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






