# POWER to ORYZA
# M. Espe
# March 2015

getPOWER <- function(lat, lon, ms = 1, ds = 1, ys,
                     me = 12, de = 31, ye)
  # Access to NASA POWER Agroclimate database from R
  # lat = latitude
  # lon = longitude
  # ms/ds/ys = month/day/year of start
  # me/de/ye = month/day/year of end
{
  require(RCurl)
  require(XML)
  u = "http://power.larc.nasa.gov/cgi-bin/cgiwrap/solar/agro.cgi"
  
  doc <- getForm(u, email = 'agroclim@larc.nasa.gov',
                 step = 1, lat = lat, lon = lon, 
                 ms = ms, ds = ds, ys = ys,
                 me = me, de = de, ye = ye,
                 submit = 'Yes')
  
  head <- readLines(textConnection(doc), n = 15)
  
  idx <- which(grepl('@ WEYR', head))
  tbl <- read.table(textConnection(doc), skip = idx)
  colnames(tbl) <- tolower(strsplit(head[idx], ' +')[[1]][-1])
  return(tbl)
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
  
  POWER_data <- getPOWER(lat = lat, lon = long, ys = year, ye = year)
  
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
  on.exit(close(ff))
  writeLines('* NASA POWER climate data formatted for ORYZA(v3)', ff)
  writeLines(paste(long, lat, 0,0,0, sep = ','), ff)
  write.table(tmp, ff, row.names = FALSE, col.names = FALSE,
              quote = FALSE, 
              eol = '\r\n', sep =",")
  
  
}

# Test functions
getwd()
# Setwd to desired location where weather files should be saved
setwd('C:/Users/mespe/Documents/CA-Variety-Trial-Dataset/ORYZA_files/AR weather files/')
POWER2ORYZA(year = 2012, station_nbr = 1, lat = 35,long =  -90, 'usar')

# Easy to do multiple years at once with sapply
sapply(2007:2014, function(x) POWER2ORYZA(x,1,34.4749,-91.4151,'usar'))
