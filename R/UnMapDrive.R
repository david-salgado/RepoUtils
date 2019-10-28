#' @name UnMapDrive
#'
#' @title Unmap a statistical operation
#'
#' @description \code{UnMapDrive} unmaps a logical drive.
#'
#' @param DriveLetter Character vector of length 1 with the letter of the logical drive.
#'
#' @param force Logical vector of length 1 specifying whether to force the unmapping or not (default
#' value: FALSE)
#'
#' @examples
#' UnMapDrive('Z:')
#'
#' @export
UnMapDrive <- function(DriveLetter = 'Z:', force = FALSE){
  
  options(warn = -1)
  cat(paste0('UnMapping logical drive ', DriveLetter, '...\n'))
  Statement <- paste('net use', DriveLetter, '/delete')
  if (force) Statement <- paste0(Statement, ' /yes')
  outShell <- shell(Statement, mustWork = TRUE, intern = TRUE)
  options(warn = 0)
  if (is.null(attributes(outShell))){
    
    cat('... ok.\n')
    cat(paste0('The logical drive ', DriveLetter, ' has been unmapped.\n\n'))
    return(TRUE)
    
  } 
  if (attributes(outShell)['status'] == '2'){
    
    cat(paste0('...The logical drive ', DriveLetter, ' was not mapped.\n\n'))
    return(TRUE)
  }
  if (attributes(outShell)['status'] == '65535'){
    
    cat(paste0('... There are open files and/or incomplete searches in the connection with ', DriveLetter,'. The connection cannot be closed.\n\n'))
    return(FALSE)
  }
  
  stop('[UnMapDrive] Check manually the mapping procedure.')
  
}
