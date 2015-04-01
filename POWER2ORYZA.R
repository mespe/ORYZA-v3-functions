# POWER to ORYZA
# M. Espe
# March 2015

get_POWER <- function(year, lat, long){
  # This returns the NASA POWER records for a single year
  # year = year of records requested
  # lat = lat of site
  # long = long of site
  require(RCurl)
  url_base <- 'http://power.larc.nasa.gov/cgi-bin/cgiwrap/solar/agro.cgi?email=agro%40larc.nasa.gov&step=1'
  url_request <- paste('&lat=', lat, 
                       '&lon=', long,
                       '&ms=3&ds=1&ys=', year,
                       '&me=11&de=30&ye=', year,
                       '&submit=Yes', 
                       sep = "")
  complete_url <- paste(url_base, url_request, sep = "")
  tmp <- getURL(complete_url)
  
  ll <- strsplit(tmp, '\n')
  
  idx <- which(grepl('@ WEYR', ll[[1]]))
  n_row <- length(ll[[1]])
  tmp_data <- matrix(NA, nrow = (n_row-idx), ncol = 11)
  for(j in 1:(n_row-idx))
    tmp_data[j,] <- strsplit(ll[[1]][(j+idx)], '[[:space:]]+')[[1]]
  colnames(tmp_data) <- tolower(strsplit(ll[[1]][idx], '[[:space:]]+')[[1]])
  tmp_data <- apply(tmp_data, 2, as.numeric)
  tmp_data <- as.data.frame(tmp_data, stringsAsFactors = FALSE)
  tmp_data['@'] <- NULL
  tmp_data
}

POWER2ORYZA <- function(year, station_nbr, lat, long, prefix){
  # This function retrieves the POWER data and
  # processes it for direct use by ORYZA(v3)
  # Weather file is saved to CURRENT WORKING DIRECTORY
  #
  # year = year of data requested
  # station_nbr = user-defined value of station number
  # lat = latitude of site
  # long = longitude of site
  # prefix = file name prefix for ORYZA weather file (e.g. - uscaXX.XXX)
  
  POWER_data <- get_POWER(year, lat, long)
  
  tmp <- data.frame(
    station_nbr = station_nbr,
    year = POWER_data$weyr,
    day = POWER_data$weday,
    srad = as.numeric(POWER_data$srad) * 1000,
    tmin = POWER_data$tmin,
    tmax = POWER_data$tmax,
    # Uses converstion formula from NOAA
    vappre = round((6.11 * 10^((7.5 * POWER_data$tdew)/(237.3 + POWER_data$tdew)))/10, 2),
    wind = POWER_data$wind,
    precip = POWER_data$rain)
  
  file_name <- paste0(prefix, station_nbr, '.', gsub('^[1|2]', '', year))
  ff <- file(file_name, 'w')
  writeLines(paste(long, lat, 0,0,0), ff)
  colnames(tmp) <- NULL
  write.table(tmp, ff, row.names=FALSE, quote = FALSE, 
              eol = '\r\n', sep =",")
  close(ff)
  
}

# Test functions
getwd()
# Setwd to desired location where weather files should be saved
setwd('C:/Users/mespe/Documents/CA-Variety-Trial-Dataset/ORYZA_files/AR weather files/')
POWER2ORYZA(2012, 1, 35, -90, 'usar')

# Easy to do multiple years at once with sapply
sapply(2007:2014, function(x) POWER2ORYZA(x,1,34.4749,-91.4151,'usar'))
