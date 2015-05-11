## Convert Stutgartt data
## Some cleaning completed by hand prior
## Data from http://www.ars.usda.gov/Main/Docs.htm?docid=23623
## M. Espe
## May 2015

## Source in the get_POWER function
source('C:/Users/mespe/ORYZA-v3-functions/POWER2ORYZA.R')

lat = 34.4
long = -91.4

getwd()
cleaned_files <- list.files(pattern = '[0-9]{4}_cleaned.csv', 
           full.names = TRUE, recursive = TRUE)

tmp <- sapply(cleaned_files, read.csv, simplify = FALSE, USE.NAMES = TRUE)

str(tmp)
length(tmp)

sapply(tmp, colnames)
year = 2012

stuttgart2oryza <- 
function(year, lat, long, station_nbr, prefix){
  x <- tmp[[grep(year, names(tmp))]]
  
  pwr <- getPOWER(lat, long, ys = year, ye = year)
  
  pwr_idx <- match(as.numeric(x$julian), pwr$weday, nomatch = 0)
  x_idx <- match(pwr$weday, as.numeric(x$julian), nomatch = 0)
  
  ## Convert to metric
  pwr$tmax[pwr_idx] <- round((x$tmaxF[x_idx] - 32) * (5/9), 2)
  pwr$tmin[pwr_idx] <- round((x$tminF[x_idx] - 32) * (5/9), 2)
  pwr$rain[pwr_idx] <- round((x$rainIN[x_idx] * 2.54), 2)
  pwr$wind[pwr_idx] <- round((x$wind[x_idx] * 0.44704), 2)
  
  ## Create temp data.frame
  tmp <- data.frame(
    station_nbr = station_nbr,
    year = pwr$weyr,
    day = pwr$weday,
    srad = as.numeric(pwr$srad) * 1000,
    tmin = pwr$tmin,
    tmax = pwr$tmax,
    # Uses converstion formula from NOAA
    vappre = round((6.11 * 10^((7.5 * pwr$tdew)/(237.3 + pwr$tdew)))/10, 2),
    wind = pwr$wind,
    precip = pwr$rain)
  
  ## Write file
  file_name <- paste0(prefix, station_nbr, '.', gsub('^[1|2]', '', year))
  ff <- file(file_name, 'w')
  on.exit(close(ff))
  writeLines(c('* AR USDA data formatted for ORYZA(v3)',
               '* Missing values filled in from NASA POWER'), ff)
  writeLines(paste(long, lat, 0,0,0, sep = ','), ff)
  write.table(tmp, ff, row.names = FALSE, col.names = FALSE,
              quote = FALSE, 
              eol = '\r\n', sep =",")
  
}

sapply(2008:2014, function(y){stuttgart2oryza(year = y, lat, long, station = 1, prefix = 'usar')})
