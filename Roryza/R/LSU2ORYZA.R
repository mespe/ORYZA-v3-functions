## Process LSU data for ORYZA
## Data downloaded from: http://weather.lsuagcenter.com/reports.aspx?r=1 8 Apr 2015
## M. Espe
## Apr 2015

## all_data <- read.csv('C:/Users/mespe/Documents/CA-Variety-Trial-Dataset/ORYZA_files/LA weather data/source files/LSU SP data 2005-2014.csv')

## all_data$DateTimeCollected <- as.Date(all_data$DateTimeCollected, '%m/%d/%Y')
## aa <- colnames(all_data)
## weird_char <- strsplit(aa[2], '')[[1]][15]

## colnames(all_data) <- gsub(weird_char, '', colnames(all_data))
## colnames(all_data) <- gsub('\\.', '', tolower(colnames(all_data)))
## all_data$julian <- format(all_data$datetimecollected, '%j')
## all_data$year <- format(all_data$datetimecollected, '%Y')

## # Change to oryza code for missing data
## all_data$minairtempf[all_data$minairtempf < -10] <- NA
## all_data$maxairtempf[all_data$maxairtempf > 130 | all_data$minairtempf < -10] <- NA

## all_date_split <- split(all_data, all_data$year)

## (table(all_data$julian, all_data$year))



LSU2ORYZA <- function(Crowley = TRUE, year, station_nbr, prefix = 'usla')
{

    x <- all_date_split[[which(as.numeric(names(all_date_split)) == year)]]
    if(Crowley){
        lat = 30.2
        long = -92.4
    }else{
        lat = 32.0
        long = -91.7
    }

    pwr <- getPOWER(lat, long, ys = year, ye = year)
    ## Set POWER temps to NA - interpolated later
    pwr$tmax <- NA
    pwr$tmin <- NA

    pwr_idx <- match(as.numeric(x$julian), pwr$weday)
    x_idx <- match(pwr$weday, as.numeric(x$julian), nomatch = 0)

    pwr$tmax[pwr_idx] <- round((x$maxairtempf[x_idx] - 32) * (5/9), 2)
    pwr$tmin[pwr_idx] <- round((x$minairtempf[x_idx] - 32) * (5/9), 2)
    pwr$rain[pwr_idx] <- round((x$rainin[x_idx] * 2.54), 2)
    pwr$wind[pwr_idx] <- round((x$avgwindspeedmph[x_idx] * 0.44704), 2)

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

createLSUweather <- function(lat, long, x, station_nbr){
  PWR <- getPOWER(lat, long, ys = x$year,
                  ye = x$year)

  tmp <- data.frame(
    station_nbr = station_nbr,
    year = x$year,
    day = x$julian,
    # Conversion from ftp://www.wcc.nrcs.usda.gov/wntsc/H&H/GEM/SolarRadConversion.pdf
    srad = as.numeric(MS_data$'SolarRadiation(langleys/day)') * 41.868,
    tmin = MS_data[,grep('Air TempMin', colnames(MS_data))],
    tmax = MS_data[,grep('Air TempMax', colnames(MS_data))],
    # Approximate vappres usinsg sat vapor = 25
    vappre = round(as.numeric(MS_data$'RelativeHumidityOB(%)') * 0.25, 3),
    wind = round(as.numeric(MS_data$'WindRun(miles/day)')/24, 2),
    precip = round(as.numeric(MS_data$'Precipitation(inches)') * 2.54, 2),
    stringsAsFactors = FALSE)

}
