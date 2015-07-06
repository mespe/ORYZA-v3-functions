## TX weather data

## beaumont <- read.table('C:/Users/mespe/Documents/CA-Variety-Trial-Dataset/ORYZA_files/TX weather data/Daily_US_TX-JEFFERSON-BEAUMONT RESEARCH CTR-2007-2015.txt',
##                        sep ='\t', header = TRUE)
## eagle <- read.table('C:/Users/mespe/Documents/CA-Variety-Trial-Dataset/ORYZA_files/TX weather data/Daily_US_TX-COLORADO-EAGLE LAKE RESEARCH STATION-2007-2015.txt',
##                     sep ='\t', header = TRUE)

## beaumont$Date <- as.Date(beaumont$Date, '%m/%d/%Y')
## eagle$Date <- as.Date(eagle$Date, '%m/%d/%Y')

## beaumont <- split(beaumont, format(beaumont$Date, '%Y'))
## eagle <- split(eagle, format(eagle$Date, '%Y'))
## lat <- 29.600948
## long <- -96.344611

## lat <- 29.600948
## long <- -96.344611
## lat <- 30.070148
## long <- -94.302316

write_TX <- function(year, station_nbr, prefix = 'ustx', lat, long){

  dd <- eagle[[which(names(eagle) == year)]]
  pwr <- getPOWER(lat = lat, lon = long, ys = year, ye = year)
  tmp <- data.frame(
    station_nbr = station_nbr,
    year = year,
    day = as.numeric(format(dd$Date, '%j')),
    srad = as.numeric(pwr$srad) * 1000,
    tmin = dd["Air.Temp.Min.ºC."],
    tmax = dd["Air.Temp.Max.ºC."],
    # Uses converstion formula from NOAA
    vappre = 0,
    wind = dd$WindSp..km.day.,
    precip = dd$Rainfall..cm.)

  # Replace NA with -99
  tmp <- apply(tmp, 2, function(x) {
    x[is.na(x)] <- -99
    x
  })

  file_name <- paste0(prefix, station_nbr, '.', gsub('^[1|2]', '', year))
  ff <- file(file_name, 'w')
  on.exit(close(ff))

  # Header lines
  writeLines('* TAMU climate data formatted for ORYZA(v3)',
             ff)

  writeLines(paste(long, lat, 0,0,0, sep = ','), ff)

  write.table(tmp, ff, row.names = FALSE, col.names = FALSE,
              quote = FALSE, sep =",")

}
