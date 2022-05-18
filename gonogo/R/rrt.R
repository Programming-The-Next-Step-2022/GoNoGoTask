#' Calculates reaction time for pressing a prespecified key
#'
#'
#'
#' @param choiceKeys A character vector specifying the key(s) for which reaction time is recorded
#'
#' @return Response (key pressed) and reaction time in seconds
#' @export
#'
#' @examples
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

