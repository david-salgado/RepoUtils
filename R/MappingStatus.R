#' @name MappingStatus
#'
#' @title  Detect if the directory of a statistical operation is mapped
#'
#' @description \code{MappingStatus} detects whether the directory of the 
#' statistical operation with IOE code SurveyCode is mapped into the logical
#' drive DriveLetter or not.
#'
#' @param SurveyCode Character vector of length 1 with the IOE code of the 
#' statistical operation.
#'
#' @param DriveLetter Character vector of length 1 with the letter of the 
#' logical drive under the common syntax [A-Z]?:.
#'
#' @return It returns a logical vector of length 1.
#'
#' @examples
#' \dontrun{
#' MappingStatus('E30183', 'Z:')
#' }
#' 
#' @export
MappingStatus <- function(SurveyCode, DriveLetter = 'Z:'){

  if (! (is.character(SurveyCode) && length(SurveyCode) == 1)) stop('[MapDrive] Only one statistical operation is allowed. SurveyCode must be a character vector of length 1.\n')
  if (! (is.character(DriveLetter) && length(SurveyCode) == 1)) stop('[MapDrive] Only one logical drive is allowed. DriveLetter must be a character vector of length 1.\n')
  if (!DriveLetter %in% paste0(LETTERS, ':')) stop('[MapDrive] DriveLetter must have the syntax A:, B:, C:, etc.')
  if (!file.exists(paste0(getwd(), '/mapdrive.exe'))) stop('[MapDrive] The executable program MapDrive.exe is not in the working directory.')
  outShell <- shell('net use', mustWork = TRUE, intern = TRUE)
  indexRowbyDriveLetter <- which(grepl(DriveLetter, outShell))
  indexRowbySurveyCode <- which(grepl(SurveyCode, outShell))
  if (length(indexRowbyDriveLetter) == 0) cat(paste0('Drive ', DriveLetter, ' is not used.\n'))
  if (length(indexRowbySurveyCode) == 0) cat(paste0('Statistical operation ', SurveyCode, ' is not mapped.\n'))
  if (length(indexRowbyDriveLetter) != 0 &&
      length(indexRowbySurveyCode) != 0 &&
      identical(indexRowbySurveyCode, indexRowbyDriveLetter)) {

    output <- TRUE

  } else {

    output <- FALSE

  }
  return(output)
}
