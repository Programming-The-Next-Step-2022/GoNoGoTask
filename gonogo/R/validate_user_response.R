#' Validates the user's response
#'
#' The function \emph{validate_user_response} validates the chosen key by comparing it to a list of predefined choice keys.
#'
#' @param choice Character string specifying the chosen key
#' @param choiceKeys Character string specifying which keys are treated as the choice keys
#'
#' @return
#' @export
#'
#' @examples
#' validate_user_response(choice = "m", choiceKeys = c("m", "o"))
#'
validate_user_response <- function(choice, choiceKeys) {
  if (choice %in% choiceKeys) {
    return(TRUE)
  } else {
    res <- FALSE
    attr(res, "message") <- paste("invalid choice, please enter one of", paste0("`", choiceKeys, "`", collapse = ", "))
    return(res)
  }
}
