#' @name MapDrive
#'
#' @title Map a statistical operation
#'
#' @description \code{MapDrive} maps a directory of the microdata repository 
#' corresponding to a statistical operation with IOE code SurveyCode into the 
#' logical drive DriveLetter.
#' 
#' A password is needed. This password is provided by the methodology unit to 
#' the head of unit of the statistical operation.
#'
#' @param SurveyCode is a character vector of length 1 with the IOE code of the 
#' statistical operation.
#'
#' @param DriveLetter is a character vector of length 1 specifying the logical 
#' drive by the common syntax [A-Z]?:.
#'
#' @param Password is a character vector of length 1.
#'
#' @examples
#' \dontrun{
#' MapDrive('E30183','Z:','xxxxxxxx')
#' }
#' 
#' @importFrom R.utils intToBin
#' 
#' @include MappingStatus.R
#'
#' @export
MapDrive <- function(SurveyCode, DriveLetter = 'Z:', PassWord){
  
  if (! (is.character(SurveyCode) && length(SurveyCode) == 1)) {
    
    stop('[MapDrive] Only one statistical operation is allowed. SurveyCode must be a character vector of length 1.\n')
    
  }
  if (! (is.character(DriveLetter) && length(SurveyCode) == 1)) {
    
    stop('[MapDrive] Only one logical drive is allowed. DriveLetter must be a character vector of length 1.\n')
    
  }
  if (!DriveLetter %in% paste0(LETTERS, ':')) {
    
    stop('[MapDrive] DriveLetter must have the syntax A:, B:, C:, etc.')
    
  }
  if (!file.exists(paste0(getwd(), '/mapdrive.exe'))) {
    
    stop('[MapDrive] The executable program MapDrive.exe is not in the working directory.')
    
  }
  if (MappingStatus(SurveyCode, DriveLetter)) {
    
    cat(paste0('The logical drive ', 
               DriveLetter, 
               ' is already temporarily mapping the repository for the statistical operation ', 
               SurveyCode, 
               '.\n\n DO NOT FORGET TO UNMAP THE DRIVE BEFORE LEAVING.\n\n'))
    return(invisible(NULL))
    
  }
  options(warn = -1)
  outShell <- shell('reg query hkey_local_machine\\software\\Microsoft\\Windows\\CurrentVersion\\Policies\\Explorer /v NoDrives', 
                    mustWork = TRUE, 
                    intern = TRUE)
  options(warn = 0)
  if (!is.null(attributes(outShell))){
    
    stop('[MapDrive] No possible drive letter is allowed with the current Windows registry configuration.\n')
    
  }
  
  outShell <- strsplit(outShell[3], '  ')[[1]]
  DWORDValueHex <- outShell[length(outShell)]
  DWORDValueDec <- as.integer(DWORDValueHex)
  HiddenDrivesindex <- strsplit(R.utils::intToBin(DWORDValueDec), 
                                integer(0))[[1]]
  HiddenDrivesindex <- as.integer(c(integer(26 - length(HiddenDrivesindex)), 
                                    HiddenDrivesindex))
  DriveValues <- 2^{0:25}
  names(DriveValues) <- paste0(LETTERS, ':')
  DriveValues <- rev(DriveValues)
  HiddenDrives <- names(DriveValues[as.logical(HiddenDrivesindex)])
  if (!DriveLetter %in% HiddenDrives){
    
    stop(paste0('[MapDrive] This drive letter is not allowed in the current Windows registry configuration.\n The allowed letters under the current configuration are ', 
                paste0(HiddenDrives, collapse= ', '), '.\n'))
    
    
  }
  
  cat(paste0('Mapping statistical operation ', 
             SurveyCode, 
             ' into logical drive ', 
             DriveLetter, 
             '...\n'))
  options(warn = -1)
  outShell <- shell(paste('mapdrive', SurveyCode, DriveLetter, PassWord), 
                    mustWork = TRUE, 
                    intern = TRUE)
  options(warn = 0)
  if (outShell[1] == 'Error de sistema 85.'){
    
    cat(paste0('...Drive letter ', 
               DriveLetter, 
               ' is currently under use. Choose another letter.\n\n'))
    return(invisible(NULL))
    
  }
  
  if (!is.null(attributes(outShell)) && attributes(outShell)['status'] == 1L){
    
    cat(paste0('... Invalid password. Enter the valid password for statistical operation ', 
               SurveyCode, 
               '.\n\n'))
    return(invisible(NULL))
  }
  
  if (outShell[1] == 'Se ha completado el comando correctamente.') {
    
    cat('... ok.\n\n')
    cat(paste0('The logical drive ', 
               DriveLetter, 
               ' is temporarily mapping the repository for the statistical operation ', 
               SurveyCode, 
               '.\n\n DO NOT FORGET TO UNMAP THE DRIVE BEFORE LEAVING.\n\n'))
    return(invisible(NULL))
    
  } 
  stop('[MapDrive] Check manually the mapping procedure.')    
  
}
