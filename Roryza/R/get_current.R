## Convienence Function to get current Oryza files
## Reads control.dat file
## M. Espe
## May 2015

get_current_oryza <-
  function(file_path)
  {
    ll <- readLines(file_path)
    ll[grepl('^FILE', ll)]
  }

