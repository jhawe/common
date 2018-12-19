#' -----------------------------------------------------------------------------
#' Starts monitoring a job based on the output on 'top'
#' bash command. Starts top in background and this process
#' should (usually) be killed manually after the job is run
#' using 'monitor_stop'
#' 
#' @param mfile The file in which to log the output of 'top'
#' @param uname The username for which to check currently running
#' processes. Defaults to 'johann.hawe'
#' 
#' @author Johann Hawe
#'
#' -----------------------------------------------------------------------------
monitor_start <- function(mfile, uname="johann.hawe") {
  # make a system call to run top in batch mode
  cmd <- paste0("top -d 10 -n 100 -b -u ", uname, " > ", mfile, " &")
  system(cmd)
}

#' -----------------------------------------------------------------------------
#'
#' This method kills currently running background jobs
#' Should always be used when monitor_start() was called earlier,
#' to get rid of this process after a job which was monitored
#' has been run
#' 
#' @author Johann Hawe
#'
#' -----------------------------------------------------------------------------
monitor_stop <- function() {
  # kill the expected one background process  (!)
  # for the current session
  system("kill -9 `jobs -p`")
}
