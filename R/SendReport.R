#' @name SendReport
#' 
#' @title Send a report  
#' 
#' @description \code{SendReport} sends a report to an email address containing 
#' information about the contents of a directory of the microdata repository.
#' 
#' @param SurveyCode is a character vector of length 1 with the IOE code of the 
#' statistical operation.
#' 
#' @param To is a character vector of length 1 with the recipient email address.
#' 
#' @param n is an integer vector of lenght 1 specifying the number of rows to be
#' included in the output. 
#' 
#' @param Extended is a logical vector of length 1 specifying whether a more 
#' extended output is to be produced.
#' 
#' @param Units is a character vector of length 1 specifying the information 
#' measure units to be used in the report (default value: 'Mb')
#'
#' @return It returns the shell response to the execution of smtpmail.exe.
#'  
#' @details The executable smtpmail.exe must be in the working directory, 
#' otherwise an error is triggered. This executable together with a necessary
#' dll library are provided by the methodology unit.  
#'  
#'  
#' @examples
#' \dontrun{
#' SendReport('E30183', 'email@example.com', n = 6L, Extended = FALSE, Units = 'Mb')
#'}
#'
#'
#' @include MappingStatus.R RepoStatus.R
#'
#' @import R2HTML StQ
#'   
#' @export
SendReport <- function(SurveyCode, To, n = 6L, Extended = FALSE, Units = 'Mb'){
  
    Drives <- paste0(rev(LETTERS), ':')
    DriveStatus <- c()
    for (Drive in Drives){
      
      auxStatus <- MappingStatus(SurveyCode, Drive)
      DriveStatus <- c(DriveStatus, auxStatus)
      if (auxStatus) break
      
    }
    
    if (!any(DriveStatus)) {
      
      stop('[SendReport] No mapped drive. Please map the statistical operation into a logical drive beforehand.')
    }
    
    Report <- RepoStatus(SurveyCode = SurveyCode, DriveLetter = Drive, 
                         Units = Units, Extended = Extended, n = n)
    FileName <- paste0(SurveyCode, '.RepoReport.', Sys.Date())
    outHTML <- HTMLInitFile(getwd(), filename = FileName)
    HTML(Report, outHTML)
    HTML('<br> <meta charset="UTF-8"> Mensaje generado autom&#225;ticamente. No responda, pues no existe buz&#243;n de respuesta.', 
         file = outHTML)
    HTML(paste0("<br>Este report est&#225; generado para los &#250;ltimos ", n, ' per&#237;odos de referencia.'), 
         file = outHTML)
    HTML(paste0("<br>Este report est&#225; generado con la opci&#243;n Extended=", Extended, '.'), 
         file = outHTML)
    HTML("<br> Si quiere modificar estos par&#225;metros, debe modificar el script de generaci&#243;n del report.", 
         file = outHTML)
    HTMLEndFile()
    cat(paste0('[SendReport] Report written in ', getwd(), '/', FileName, '.html\n\n'))

    outShell <- list()
    for (Recipient in To){
      
      outShell[[Recipient]] <- shell(paste0('SMTPMAIL.EXE from=repomicrodatos@ine.es ',
                                            'to=', Recipient,
                                            ' subject="[Repo ', SurveyCode, '] Report ', Sys.time(), '"',
                                            ' body="', paste0('                 ESTADO DEL REPOSITORIO PARA LA OPERACIÓN ESTADÍSTICA ', SurveyCode), '"',
                                            ' bodyfile=', paste0(getwd(), '/', FileName, '.html'),
                                            ' server=remesas.ine.es'),
                                    mustWork = TRUE, 
                                    intern = TRUE)
      
    }
    outShell <- lapply(outShell, '[', 5)
    return(outShell)    
    

}