#' @title Report status of the repository 
#' 
#' @description \code{RepoStatus} gives information about the Status of the 
#' mapped repository.
#' 
#' @param SurveyCode Charcter vector of length 1 with the code of the 
#' statistical operation.
#'
#' @param DriveLetter Character vector of length 1 with the letter of the 
#' logical drive (default value \code{Z:}).
#' 
#' @param Units Character vector length 1 with the information unit measure to 
#' use (default value \code{Mb}).
#' 
#' @param Extended Logical vector of length 1 expresing whether an extended 
#' report muest be produced or not.
#'
#' @param n Integer vector of length 1 with the number of files (backwards in 
#' time sequence) to report about. 
#' 
#' @return It returns a list of \linkS4class{data.table}s for each type of file
#' in the repository with columns \code{File} (name of the file), 
#' \code{Reference} (time period of reference in the repository notation), 
#' \code{Size} (in the specified units), \code{Created} (date of creation in 
#' format dd/mm/yyyy) and \code{Last accessed} (date of of last accession in
#' format dd/mm/yyyy). If \code{Extended = TRUE}, they also include columns 
#' \code{Rows} (the number of rows, except the first line) and \code{Units} 
#' (with the number of statistical units in each file).
#'  
#' @examples
#' \dontrun{
#' RepoStatus('E30183', DriveLetter = 'Z:', Units = 'Mb', Extended = FALSE, n = 6L)
#' }
#' 
#' @include MappingStatus.R 
#'
#' @import RepoTime RepoReadWrite StQ data.table xlsx
#'   
#' @export
RepoStatus <- function(SurveyCode, 
                       DriveLetter = 'Z:', 
                       Units = 'Mb', 
                       Extended = FALSE, 
                       n = 6L){
  
  if (!MappingStatus(SurveyCode, DriveLetter)) {
    
    stop(paste0('[RepoStatus] ', SurveyCode, ' is not mapped to drive ', DriveLetter, '. Please map drive beforehand.\n'))
    
  }
  if (!(is.integer(n) && n >= 1)) stop('[RepoStatus] n must be a positive integer. Specify 1L, 2L, 3L, ...')
  
  UnitConvFactor <- if (Units == 'b') {
    
    1
    
  } else if (Units == 'Kb'){
    
    1024
    
  } else if (Units == 'Mb'){
    
    1024 * 1024
    
  } else if (Units == 'Gb'){
    
    1024 * 1024 * 1024
    
  } else stop('[RepoStatus] Units not recognized. Specify b, Kb, Mb (default) or Gb.')
  
  FileNames <- list.files(paste0(DriveLetter, '/'))
  ParsedFileNames <- strsplit(FileNames, '.', fixed = TRUE)
  
  Types <- unlist(lapply(ParsedFileNames, '[', 2))
  Types <- unlist(lapply(strsplit(Types, '_'), '[', 1))
  ValidTypes <- c('DD', 'FF', 'FD', 'FG', 'FL', 'FT', 
                  'NombresVariables', 'ParamFL', 'ParamFT')
  indexValidTypes <- (Types %in% ValidTypes )
  Types <- unique(Types[indexValidTypes])
  
  Periods <- unlist(lapply(ParsedFileNames, '[', 3))
  
  output <- list()

  for (Type in Types){
    
      indexlocalFileNames <- grep(paste0('.', Type), FileNames, fixed = TRUE)
      localFileNames <- FileNames[indexlocalFileNames]
      output[[Type]] <- data.table(File = localFileNames)
      output[[Type]][, Reference := Periods[indexlocalFileNames]]
      
      if (Type=='DD'){
        
        RefOrder <- seq(along = output[[Type]][['Reference']])
        
      }
      
      else if (Type == 'ParamFL'){
        
        x <- output[[Type]]
        index <- grep('ParamFL', x$File, fixed = TRUE)
        auxSplit <- strsplit(x$File[index], '.', fixed = TRUE)
        auxSplit <- unlist(lapply(auxSplit, '[', 4))
        x$Reference[index] <- auxSplit
        output[[Type]] <- x

        lubriInterval <- RepoTimeTolubri(output[[Type]][['Reference']])
        startRef <- unlist(lapply(lubriInterval, slot, 'start'))
        RefOrder <- order(startRef, decreasing = TRUE)
        
      }
      else if (Type == 'FT') {
        
        x <- output[[Type]]
        index <- grep('FT_V1', x$File, fixed = TRUE)
        output[[Type]] <- x[index]
        
        lubriInterval <- RepoTimeTolubri(output[[Type]][['Reference']])
        startRef <- unlist(lapply(lubriInterval, slot, 'start'))
        RefOrder <- order(startRef, decreasing = TRUE)
        
      } 
      else if (Type == 'NombresVariables') {
        
        output[[Type]][['Reference']] <- NA
        RefOrder <- seq(along = output[[Type]][['Reference']])
        
      } else {
      
        lubriInterval <- RepoTimeTolubri(output[[Type]][['Reference']])
        startRef <- unlist(lapply(lubriInterval, slot, 'start'))
        RefOrder <- order(startRef, decreasing = TRUE)
        
      }
      
      output[[Type]] <- output[[Type]][RefOrder,]
    
      nlocal <- min(n, length(localFileNames))
      output[[Type]] <- output[[Type]][1:nlocal]
      localFileNames <- output[[Type]][['File']]
      auxFileInfo <- file.info(paste0(DriveLetter, '/', localFileNames))[, c('size', 'ctime', 'atime')]

      output[[Type]][, c('size', 'ctime', 'atime') := auxFileInfo] 
      
      output[[Type]][['size']] <- round(output[[Type]][['size']] / UnitConvFactor, 1)
      
      output[[Type]][['ctime']] <- format(as.Date(output[[Type]][['ctime']]), "%d/%m/%Y")
      output[[Type]][['atime']] <- format(as.Date(output[[Type]][['atime']]), "%d/%m/%Y")
      setnames(output[[Type]], c('File', 'Reference', paste0('Size (', Units, ')'), 'Created', 'Last accessed'))
  }

  if (Extended){
    
    #####                       LEER HOJAS DE EXCEL                            #####
    
    ExcelNames <- FileNames[grep('NombresVariables', FileNames, fixed = TRUE)]
    ExcelVersions <- lapply(ExcelNames, function(Name){strsplit(Name, split = '_V', fixed = TRUE)[[1]][2]})
    ExcelVersions <- lapply(ExcelVersions, function(x){strsplit(x, split = '.', fixed = TRUE)[[1]][1]})
    ExcelName <- paste0(DriveLetter, '/', ExcelNames[which.max(ExcelVersions)])
    DD <- RepoXLSToDD(ExcelName)

    for (Type in setdiff(names(output), c('DD', 'NombresVariables'))){
      
      if (Type %in% c('FF', 'FD', 'FG', 'FL', 'FT')){
        Rows <- c()
        Units <- c()
        for (i in seq(along = output[[Type]]$File)){
          cat(paste0('Reading file ', output[[Type]]$File[i]), '\n')
          auxStQ <- ReadRepoFile(paste0(DriveLetter, output[[Type]]$File[i]), DD, perl = TRUE)
          nRows <- dim(auxStQ)[[1]]
          Rows <- c(Rows, nRows)
          nUnits <- dim(getUnits(auxStQ))[1]
          Units <- c(Units, nUnits)
        }
        output[[Type]][['Rows']] <- Rows
        output[[Type]][['Units']] <- Units
      }
    }
  }
  
  return(output)
  
}