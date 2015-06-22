## Convienence Function to get current Oryza files
## Reads control.dat file
## M. Espe
## May 2015

##' Get the current files being referred to in an Oryza(v3) control file
##'
##' This is a convienence function that reads the Oryza(v3)
##'    control file specified and prints the current active files
##'    to the console.
##'
##' @title Get current Oryza settings
##' @param file_path complete file path to control file, including
##'     the name of the file to be read.
##'
##' @return active files being referred to in control file
##'
##' @author Matthew Espe
##'
get_current_oryza <-
  function(file_path)
  {
    ll <- readLines(file_path)
    ll[grepl('^FILE', ll)]
  }

