# create_RERUN
# M. Espe
# Feb. 2015

# Make test dataframe
# IYEAR = rep(c(2012, 2013), 8)
# EMYR = IYEAR
# STTIME = paste0(seq(120,135), '.')
# EMD = seq(120,135)
# FINTIM = seq(260,275)
# ISTN <- rep(c(12,32,6), length.out = 16)

#rerun_data <- data.frame(IYEAR = IYEAR,
#                         EMYR = EMYR,
#                         STTIME = STTIME,
#                         EMD = EMD,
#                         FINTIM = FINTIM,
#                         ISTN = ISTN,
#                         stringsAsFactors = FALSE)

# Define function

create_RERUN <- function(rerun_data, file_path){
  # creates an ORYZA(v3) rerun file
  # rerun_data = dataframe with each column a variable for the rerun
  #               IMPORTANT!: Column names must match ORYZA variable names
  # file_path = location and name where file should be created

  variables <- colnames(rerun_data)

  tmp <- sapply(1:nrow(rerun_data), function(i) {
    (c(paste('* Rerun: ', i), paste(variables, rerun_data[i,], sep = " = "), '\n'))
  })
  writeLines(tmp, file_path)
}

# test it out
# create_RERUN(rerun_data, '~/test.rer')
# readLines('~/test.rer')
