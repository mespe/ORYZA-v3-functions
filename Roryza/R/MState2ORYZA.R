# Aquire Miss. State weather
# M. Espe
# Apr 2015

getMSweather <- function(start_date, end_date, station)
{

  u = "http://ext.msstate.edu/anr/drec/stations.cgi"

  doc <- getForm(u,
                 station = station,
                 start_date = start_date,
                 end_date = end_date,
                 submit = 'View All Data',
                 report = 'AIR TEMPERATURE')
  tmp <- readHTMLTable(doc, stringsAsFactors = FALSE,
                blank.lines.skip = TRUE)[[1]]
  idx <- which(grepl('^$|Sum|Average', tmp[,1]))
  tmp[-idx,]
}

MS2ORYZA <- function(year, station, station_nbr, prefix = 'usms'){
  # Retrieves weather data from MS State and
  # Processes for ORYZA(v3)
  # year = year requested
  # station = Stoneville or Lyon
  # station_nbr = user defined number for station
  # prefix = user defined prefix in ORYZA weather file format

  MS_data <- getMSweather(start_date = paste0('1/1/', year),
                          end_date = paste0('12/31/', year),
                          station = station)

  tmp <- suppressWarnings(
    data.frame(
    station_nbr = station_nbr,
    year = year,
    day = MS_data$JulianDate,
    # Conversion from ftp://www.wcc.nrcs.usda.gov/wntsc/H&H/GEM/SolarRadConversion.pdf
    srad = as.numeric(MS_data$'SolarRadiation(langleys/day)') * 41.868,
    tmin = MS_data[,grep('Air TempMin', colnames(MS_data))],
    tmax = MS_data[,grep('Air TempMax', colnames(MS_data))],
    # Approximate vappres usinsg sat vapor = 25
    vappre = round(as.numeric(MS_data$'RelativeHumidityOB(%)') * 0.25, 3),
    wind = round(as.numeric(MS_data$'WindRun(miles/day)')/24, 2),
    precip = round(as.numeric(MS_data$'Precipitation(inches)') * 2.54, 2),
    stringsAsFactors = FALSE)
  )

  # Replace NA with -99
  tmp <- apply(tmp, 2, function(x) {
    x[is.na(x)] <- -99
    x
    })

  file_name <- paste0(prefix, station_nbr, '.', gsub('^[1|2]', '', year))
  ff <- file(file_name, 'w')
  on.exit(close(ff))

  # Header lines
  writeLines(c('* MS State climate data formatted for ORYZA(v3)',
               paste('*', station, year, sep = ' '),
               paste('* Created: ', date())), ff)

  # Approximate location
  if(station == 'Stoneville'){
    writeLines(paste(33.4, -90.9, 0,0,0, sep = ','), ff)
  } else{
    writeLines(paste(34.2, -90.5, 0,0,0, sep = ','), ff)
  }
  write.table(tmp, ff, row.names = FALSE, col.names = FALSE,
              quote = FALSE,
              eol = '\r\n', sep =",")
}



