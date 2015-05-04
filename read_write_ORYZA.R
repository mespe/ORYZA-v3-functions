# Functions to read ORYZA output in R
# M. Espe
# March 2015

# This is a collection of functions that process res.dat and op.dat files
# that are produced by ORYZA(v3)


# read in res.dat file

read_res <- function(file_path){
  # Returns a single dataframe of the res.dat data
  # Uses cygwin shell commands to pre-process the res.dat file
  # file_path = the file path to the directory containing the res.dat file
  
  
  con <- pipe('cut -f 1,2 -s res.dat')
  on.exit(close(con))
  ll <- readLines(con)
  breaks <- grepl('(^$)|(^[*])|^TIME|(^ $)|WARNING|Please', ll)
  tmp <- ll[!breaks]
  
  tmp <- read.table(textConnection(tmp), sep = '\t')
  
  return(tmp)
}

# Split into reruns

split_res <- function(res_dataframe){
  # Splits the res dataframe into a list with each rerun as an element
  # Only works when the crop does not overlap with a new year
  # res_dataframe = object created by read_res()
  aa <- res_dataframe$V1
  idx <- c(0, which((aa - c(aa[-1], 1)) != -1))
  
  bb <- NA
  for(i in 2:length(idx)){bb[(i-1)] <- idx[i] - idx[(i-1)]}
  
  split_vect <- rep(1:length(bb), times = bb)
  
  reruns <- split(res_dataframe, split_vect)
  return(reruns)
}

# find rows with closest match to phenology dates

find_phen_pred <- function(reruns, DAP = FALSE){
  # Find the predicted dates of each phenological event
  # reruns = split reruns file produced by split_res()
  # DAP = days after planting(simulation start) otherwise gives day of year
  
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

# Wrapper function for previous functions

get_pred_mat <- function(file_path, DAP = TRUE){
  # function to read model output and return predictions
  # This discards first run (run of .exp file)
  # file_path = location of output files
  
  # Go to file locations
  old_dir <- getwd()
  if(old_dir != file_path) setwd(file_path)
  on.exit(if(getwd() != old_dir) setwd(old_dir))
  
  tmp <- read_res(file_path)
  tmp <- split_res(tmp)
  tmp <- find_phen_pred(tmp, DAP=TRUE)
  yield <- read.table('op.dat', header = TRUE)$WRR14
  
  if(length(tmp[,1]) != length(yield)) {
    warning('At least one simulation did not proceed to maturity!')
    return(NULL)
  }
  
  # Discard first run
  tmp <- cbind(tmp[-1,], yield = yield[-1])
  
  # Reset working directory
  if(old_dir != file_path) setwd(old_dir)
  return(tmp)
}

