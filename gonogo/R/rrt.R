#' Calculates the reaction time for pressing a key within a time interval
#'
#' @param choiceKeys The key(s) for which reaction time is recorded (default is the spacebar)
#' @param interval The interval within which the reaction time is recorded; if the key is not pressed within the interval, response is recorded as none and reaction time as a missing value (default is 0.6 seconds)
#'
#' @return The response (either the choice key or "none"), and the rt (either reaction time in seconds or a missing value)
#'
#' @examples
#' rrt(choiceKeys = c(" "), interval = 1.5)
rrt <- function(choiceKeys = c(" "), interval = 0.6) {
  
  dynamic_readline <- function() {
    x <- 0
    while (rstudioapi::isAvailable()) {
      input <- rstudioapi::getConsoleEditorContext()$contents
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
  
  rt <- Sys.time()
  repeat {
      choice <- dynamic_readline()
    if (choice %in% choiceKeys) {
      rt <- Sys.time() - rt
      break
    }
    elapsed_time <- Sys.time() - rt
    if (elapsed_time > interval) {
      rt <- NA
      break
    }
  }
  setNames(c(choice, rt), c("response", "rt"))
  
}
