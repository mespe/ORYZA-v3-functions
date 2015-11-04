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

MS2ORYZA <- function(year, station, station_nbr){
    ## Retrieves weather data from MS State and
    ## Processes for ORYZA(v3)
    ## year = year requested
    ## station = Stoneville or Lyon
    ## station_nbr = user defined number for station
    ## prefix = user defined prefix in ORYZA weather file format

    x <- getMSweather(start_date = paste0('1/1/', year),
                      end_date = paste0('12/31/', year),
                      station = "Stoneville")

    if(station == "Stoneville"){
        lat = 33.4
        long = -90.9
    }else{
        lat = 34.2
        long = -90.5
    }

    pwr <- getPOWER(lat, long, ys = year, ye = year)
    ## Set POWER temp to NA - interpolate later
    pwr[c('tmin','tmax')] <- NA

    pwr_idx <- match(as.numeric(x$JulianDate), pwr$weday)
    x_idx <- match(pwr$weday, as.numeric(x$JulianDate), nomatch = 0)

    pwr$tmax[pwr_idx] <- round((as.numeric(
                                   x[,grep('Air TempMax', colnames(x))][x_idx]) - 32) * (5/9), 2)
    pwr$tmin[pwr_idx] <- round((as.numeric(
                                   x[,grep('Air TempMin', colnames(x))][x_idx]) - 32) * (5/9), 2)
    pwr$rain[pwr_idx] <- round(as.numeric(x$'Precipitation(inches)')[x_idx] * 2.54, 2)
    pwr$wind[pwr_idx] <- round(as.numeric(x$'WindRun(miles/day)')[x_idx]/24, 2)

    tmp <- data.frame(
        station_nbr = station_nbr,
        year = pwr$weyr,
        day = pwr$weday,
        srad = as.numeric(pwr$srad) * 1000,
        tmin = pwr$tmin,
        tmax = pwr$tmax,
        ## Uses converstion formula from NOAA
        vappre = round((6.11 * 10^((7.5 * pwr$tdew)/(237.3 + pwr$tdew)))/10, 2),
        wind = pwr$wind,
        precip = pwr$rain)

    return(tmp)
}



