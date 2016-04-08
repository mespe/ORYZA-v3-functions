# Functions to read ORYZA output in R
# M. Espe
# March 2015

# This is a collection of functions that process res.dat and op.dat files
# that are produced by ORYZA(v3)


#
##' Read in res.dat file
##'
##' This function uses cygwin commands to pre-process the res.dat file
##'     that is produced by Oryza(v3). Will not work if current working
##'     directory does not contain the res.dat file or if output is
##'     named differently than res.dat.
##'
##' @title Read res.dat
##' @return a single \code{data.frame} object with all data rows
##'     from the res.dat file.
##'
##' @author Matthew Espe
##'
read_res <- function(){
  con <- pipe('cut -f 1,2 -s res.dat')
  on.exit(close(con))
  ll <- readLines(con)
  breaks <- grepl('(^$)|(^[*])|^TIME|(^ $)|WARNING|Please', ll)
  tmp <- ll[!breaks]

  tmp <- read.table(textConnection(tmp), sep = '\t')

  return(tmp)
}

##' Split into reruns
##'
##' Splits the res dataframe into a list with each rerun as an element.
##'
##' @title split res
##' @param res_dataframe a \code{data.frame} containing the data from the res.dat
##'     file - created by \code{read_res}
##'
##' @return a \code{list} object with each element corresponding to a rerun
##'     from Oryza(v3)
##'
##' @author Matthew Espe
##'
split_res <- function(res_dataframe, var){
  aa <- res_dataframe[[var]]
  idx <- c(0, which((aa - c(aa[-1], 1)) > 0))

  bb <- NA
  for(i in 2:length(idx)){bb[(i-1)] <- idx[i] - idx[(i-1)]}

  split_vect <- rep(1:length(bb), times = bb)

  reruns <- split(res_dataframe, split_vect)
  return(reruns)
}

##' Find phenology predictions from Oryza
##'
##' Find the predicted dates of each phenological event
##'     reruns = split reruns file produced by split_res()
##'     DAP = days after planting(simulation start) otherwise gives day of year
##'
##' @title Find phenology predictions
##'
##' @param reruns a \code{list} object, each element corresponding to
##'     rerun - produced by \code{split_res}
##' @param DAP logical - should results be returned as day of year
##'     or as DAP (days after planting)
##'
##' @return a \code{matrix} with 3 columns: prediced panicle initiation,
##'     50% heading, and physiological maturity.  Each row corresponds
##'     to the a single rerun in the res.dat file.
##'
##' @author Matthew Espe
##'
find_phen_pred <- function(reruns, DAP = FALSE){

  phen_pred <- t(sapply(reruns, function(x){
    aa <- x$V2
    DOY <- rep(NA, 3)
    DOY[1] <- which(aa > 0.65)[1]
    DOY[2] <- which(aa > 1)[1]
    DOY[3] <- which.max(aa)
    return(DOY)
  }))

  colnames(phen_pred) <- c('PI', 'flower', 'PM')

  if(DAP){
    return(phen_pred)
  }

  for(i in seq_along(reruns)){
    phen_pred[i,] <- reruns[[i]]$V1[phen_pred[i,]]
  }

  return(phen_pred)
}

##' Get prediciton matrix
##'
##' Function to read model output and return predictions.
##'     This discards the first run (run of .exp file).
##'
##' @title Get matrix of Oryza(v3) predictions
##'
##' @param file_path character string of file-path to location where
##'    res.dat and op.dat are currently saved.
##' @param DAP logical - should results be return as day of year, or
##'     DAP (days after planting).
##'
##' @return a \code{matrix} with 4 columns: prediced panicle initiation,
##'     50% heading, physiological maturity, and yield.  Each row corresponds
##'     to the a single rerun in the res.dat file.
##'
##' @author Matthew Espe
##'
get_pred_mat <- function(file_path, DAP = TRUE){
  # Go to file locations - reset working dir on exit
  old_dir <- getwd()
  if(old_dir != file_path) setwd(file_path)
  on.exit(if(getwd() != old_dir) setwd(old_dir))

  tmp <- read_res()
  tmp <- split_res(tmp, 'V2')
  tmp <- find_phen_pred(tmp, DAP = DAP)
  yield <- read.table('op.dat', header = TRUE)$WRR14

  if(length(tmp[,1]) != length(yield)) {
    warning('At least one simulation did not proceed to maturity!')
    return(NULL)
  }

  # Discard first run
  tmp <- cbind(tmp[-1,], yield = yield[-1])

  return(tmp)
}

