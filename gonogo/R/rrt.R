#' Calculates the reaction time for pressing a key within a time interval
#'
#' @param choice_keys The key(s) for which reaction time is recorded 
#'                   (default is the space bar)
#'                   
#' @param interval The interval within which the reaction time is recorded;
#'                 if the key is not pressed within the interval, response 
#'                 is recorded as none and reaction time as a missing value
#'                 (default is 0.6 seconds)
#'
#' @return Returns the response (either the choice key or "none"), and the 
#'         rt (either reaction time in seconds or a missing value)
#'
#' @examples
#' rrt(choice_keys = c(" "), interval = 1.5)
rrt <- function(choice_keys = c(" "), interval = 0.6) {
  
  # Check class and length of arguments
  if (class(choice_keys) != "character" || length(choice_keys) < 1) {
    stop("choice_keys must be a character vector of length (at least) 1")
  }
  if (class(interval) != "numeric" || length(interval) != 1) {
    stop("interval must be a numeric vector of length 1")
  }
  
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
    if (choice %in% choice_keys) {
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
