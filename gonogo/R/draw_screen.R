#' Draws a screen
#' 
#' The \emph{draw_screen} function is a helper function for the 
#' \emph{play_gonogo} function. It plots a screen onto which the
#' instructions and stimuli can be plotted.
#'
#' @param txt A character string specifying the text to be printed on 
#'            the screen
#' @param cex A numeric value specifying the font size (default is 1)
#' @param col A character string specifying the color of the text 
#'            (default is black)
#'            
#' @export
#'
#' @return Draws a screen corresponding to the specified values
#'
#' @examples
#' draw_screen(txt = "Hello!", cex = 2, col = "red")
draw_screen <- function(txt, cex = 1, col = "black") {
  
  # Check that arguments are in the correct format
  if (class(txt) != "character" || length(txt) != 1) {
    stop("txt must be a character vector of length 1")
  }
  if (class(cex) != "numeric" || length(cex) != 1) {
    stop("cex must be a numeric vector of length 1")
  }
  if (class(col) != "character" || sum("black" == colours()) != 1) {
    stop("col must be a character vector of length 1, and it must match with one element of the built-in colours that R knows about")
  }
  
  plot(x = NA, y = NA, 
       xlim = c(0, 1), ylim = c(0, 1), 
       xaxt = "n", yaxt = "n", 
       xlab = "", ylab = "")
  text(x = 0.5, y = 0.5, labels = txt, cex = cex)
  
}
