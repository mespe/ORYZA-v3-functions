# Edit ORYZA(v3) files from R
# M. Espe
# March 2015


edit_ORYZA_file <- function(variables, values, file_path){
  # Finds variables of interest and re-writes them with new values
  # variables = vector of variable names
  # values = new values, in order of variables
  # file_path = location of file to be edited

  con <- file(file_path, open = 'rt')
  on.exit(close(con))

  dd <- readLines(con)
  for(i in seq_along(variables)){
    idx <- which(grepl(variables[i], dd))
    dd[idx] <- paste(variables[i], ' = ', values[i], sep = '')
  }

  writeLines(text = dd, file_path)
}

# Test
# Re-write crop file
#variable <- c('DVRJ', 'DVRI')
#value <- c(0.0010356, 0.0007576)
#file_path <- 'C:/Users/mespe/Documents/CA-Variety-Trial-Dataset/ORYZA_files/M205.crp'


# Call ORYZA from within R
# Must set working directory to ORYZA dir
#setwd('C:/ORYZA(v3)/')
#system2('C:/ORYZA(v3)/ORYZA3.exe')


