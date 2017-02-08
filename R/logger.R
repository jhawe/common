#'
#' Script to conveniently log information
#'
#' @author Johann Hawe
#' 
#' @date 07/02/2017
#' 


#' Creates an instance of a logger
#' 
#' @param log.file The full path to the log file to be used. In case no file is
#' provided, a temp file will be created in log.dir
#' @param log.dir The full path to the directory in which to create the log file
#' if log.file=NULL. If no directory is provided and log.file=NULL, a new temp 
#' directory will be created as well as a log file within it to which logs will 
#' be written 
#' @param fileext The file extension to be used when creating the file name for 
#' the log file in case log.file=NULL
#' 
logger.create <- function(log.file=NULL, log.dir=NULL, fileext=".log"){
  if(log.file=NULL) {
    pat <- format(Sys.time(), format="%a%d%b%Y_%H%M%S");
    if(log.dir=NULL){
      log.file <- tempfile(pat, fileext = fileext);
      log.dir <- dirname(log.file);
    } else {
      log.file <- tempfile(pat, tmpdir = log.dir, fileext = fileext);
    }
  }
  
  logger <- list(log.file=log.file, log.dir=log.dir);
  class(logger) <- "logger"
  return(logger);
}

#' Logs a message
#' 
#' @param msg The message to be logged
#' @param sep The separator to be used between individual messages
#' @param logger An instance of a logger. If null, message will be logged to 
#' std-out
#' 
#' @author Johann Hawe
#' 
logger.log <- function(msg, sep="\n", logger=NULL) {
  if(!is.null(logger)){
    if(class(logger) != "logger"){
      stop("Provided logger is of the wrong class!.");
    }
    con <- logger$log.file;
  } else {
    # lets cat print to std-out
    con <- "";
  }
  cat(file=con,sep=sep,msg)
}
