## TX weather data

## beaumont <- read.table('C:/Users/mespe/Documents/CA-Variety-Trial-Dataset/ORYZA_files/TX weather data/Daily_US_TX-JEFFERSON-BEAUMONT RESEARCH CTR-2007-2015.txt',
##                        sep ='\t', header = TRUE)
## eagle <- read.table('C:/Users/mespe/Documents/CA-Variety-Trial-Dataset/ORYZA_files/TX weather data/Daily_US_TX-COLORADO-EAGLE LAKE RESEARCH STATION-2007-2015.txt',
##                     sep ='\t', header = TRUE)

## beaumont$Date <- as.Date(beaumont$Date, '%m/%d/%Y')
## eagle <- split(eagle, format(eagle$Date, '%Y'))
## lat <- 29.600948
## long <- -96.344611

## lat <- 29.600948
## long <- -96.344611
## lat <- 30.070148
## long <- -94.302316

sat_vp <- function(x) {
    0.6108 * exp((17.27 * x)/(x +237.3))
}

write_TX <- function(weather_data, year,
                     station_nbr, prefix = 'ustx', lat, long){

    dd <- weather_data[[which(names(weather_data) == year)]]
    pwr <- getPOWER(lat = lat, lon = long, ys = year, ye = year)

    tmp <- data.frame(
        station_nbr = station_nbr,
        year = year,
        day = as.numeric(format(dd$Date, '%j')),
        srad = as.numeric(pwr$srad) * 1000,
        tmin = dd[,3],
        tmax = dd[,2],
        ## Uses converstion formula from NOAA
        vappre = ((sat_vp(dd[,2]) + sat_vp(dd[,3]))/2) * (((
            dd[,7] + dd[,8])/200)),
        ## convert from km/day to m/s
        wind = dd$WindSp..km.day. * 0.01157,
        ## convert to mm
        precip = dd$Rainfall..cm. * 10)

 return(tmp)
}
