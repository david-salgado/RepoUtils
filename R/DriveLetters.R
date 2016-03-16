#' @name DriveLetters
#'
#' @title Drive Letters
#'
#' @description \code{DriveLetters} returns a character vector with the letters 
#' of the logical drives in use.
#'
#' @return Returns a character vector with components [A-Z]?:.
#'
#' @examples
#' DriveLetters()
#'
#' @export
DriveLetters <- function(){
    outShell <- shell('wmic logicaldisk get caption', 
                      mustWork = TRUE, 
                      intern = TRUE)
    outShell <- outShell[-1]
    outShell <- outShell[-length(outShell)]
    output <- substr(outShell, start = 1, stop = 2)
    return(output)
}
