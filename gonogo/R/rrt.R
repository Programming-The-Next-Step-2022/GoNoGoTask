#' Calculates reaction time for pressing a prespecified key
#'
#'
#'
#' @param choiceKeys A character vector specifying the choice key(s) for which the reaction time is recorded
#'
#' @param interval A numeric vector of length 1 specifying the length of each trial in seconds (default is 1)
#'
#' @return Response (either choice key pressed, or NA) and reaction time in seconds (NA if choice key not pressed)
#' @export
#'
#' @examples
rrt <- function(choiceKeys=c(" "), interval = 0.6) {
  dynamic_readline <- function() {
    # Create a counter variable that breaks the loop
    x <- 0
    while (rstudioapi::isAvailable()) {
      input <- rstudioapi::getConsoleEditorContext()$contents
      # Increase counter variable
      x <- x + 1
      if (input != "") {
        rstudioapi::sendToConsole("", execute = FALSE)
        return(input)
      }
      if (x == 5) {
        return("none")
      }
    }
    readline()
  }
  o_rt <- Sys.time()
  repeat {
      choice <- dynamic_readline()
    if (choice %in% choiceKeys){
      o_rt <- Sys.time() - o_rt
      break
    }
    elapsed_time <- Sys.time() - o_rt
    if (elapsed_time > interval) {
      o_rt <- 100
      break
    }
  }
  setNames(c(choice, o_rt), c("response", "rt"))
}
