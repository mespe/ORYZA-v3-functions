## Get NOAA data
## M. Espe
## June 2015

## library(rnoaa)
## api_key <- "gFZdLcyneuOPFijFzVBgMXtFVXUuFNGC"

## out <- ncdc(datasetid = 'GHCND', stationid = 'GHCND:USC00031632',
##             startdate = '2010-02-01', enddate = '2010-11-30', token = api_key,
##             limit = 1000, datatypeid = c('TMIN', 'TMAX'))

## ncdc_datatypes(datasetid = 'GHCND', token = api_key)
## ncdc_datasets(token = api_key)
## str(out)
## head(out$data)
## startdate = '2010-01-01'
## enddate = '2010-12-31'
## lat = 36.4197
## long = -90.5858

ncdc2ORYZA <- function(api_key, year, station_nbr, prefix, lat, long)
{
  temps <- ncdc(datasetid = 'GHCND', stationid = 'GHCND:USC00031632',
                startdate = paste(year, '-01-01', sep = ''),
                enddate = paste(year, '-12-31', sep = ''),
                token = api_key,
                limit = 1000, datatypeid = c('TMIN', 'TMAX'))$data
  tmin <- temps$value[temps$datatype == 'TMIN']
  tmax <- temps$value[temps$datatype == 'TMAX']

  j <- as.numeric(
    unique(
      format(as.Date(gsub('T00:00:00', '', temps$date)), '%j')))

  pwr <- Roryza:::getPOWER(lat, long, ys = year, ye = year)

  pwr_idx <- match(j, pwr$weday, nomatch = 0)
  x_idx <- match(pwr$weday, j, nomatch = 0)

  pwr$tmax[pwr_idx] <- tmax[x_idx]/10
  pwr$tmin[pwr_idx] <- tmin[x_idx]/10

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

  file_name <- paste0(prefix, station_nbr, '.', gsub('^[1|2]', '', year))
  ff <- file(file_name, 'w')
  on.exit(close(ff))
  writeLines(c('* NOAA temp data formatted for ORYZA(v3)',
               '* missing values and sol-rad, wind, and precip',
               '* from NASA POWER'),
             ff)
  writeLines(paste(long, lat, 0,0,0, sep = ','), ff)
  write.table(tmp, ff, row.names = FALSE, col.names = FALSE,
              quote = FALSE, sep =",")
}
