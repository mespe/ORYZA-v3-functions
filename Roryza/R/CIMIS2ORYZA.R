## Function to query CIMIS weather data
## M. Espe
## 2015

library(RCurl)
library(jsonlite)

## Key is stored separate and not shared
#api_key <- readLines('~/Dropbox/CIMIS API key')

## Define function

##' Query the CIMIS API
##'
##' .. content for \details{} ..
##' @title Get CIMIS data
##' @param api_key a personal API key for accessing data
##' @param ... additional arguments in the form of "key = value" pairs
##'     passed along as query parameters
##' @param start start date, in ISO format ("YYYY-mm-dd")
##' @param end end date, in ISO format
##' @param url default URL to query
##' @return a data.frame object of query results
##'
##' @author Matthew Espe
##'
getCIMIS <- function(api_key, ...,
                     start, end,
                     url = "http://et.water.ca.gov/api/data")
{
  doc <- getForm(uri = url, appKey = api_key,
                 startDate = start, endDate = end,  ...)
  fromJSON(doc, flatten = TRUE)$Data$Providers$Records[[1]]
}

CIMIS2Oryza <- function(api_key, year, station_nbr)
{
  tmp <- getCIMIS(api_key, 
                  start = paste0(year, '-01-01'), 
                  end = paste0(year, '-12-31'), 
                  unitOfMeasure = 'M',
                  targets = station_nbr)
  
  idx <- grep('[.]Qc$|[.]Unit$', colnames(tmp))
  tmp <- tmp[,-idx]
  x <- colnames(tmp)
  
  data <- data.frame(
    station_nbr = station_nbr,
    year = year,
    doy = tmp[,grep('Julian', x)],
    solrad = NA,
    tmin = tmp[,grep('DayAirTmpMin', x)],
    tmax = tmp[,grep('DayAirTmpMax', x)],
    vp = tmp[,grep('DayVapPresAvg', x)],
    wind = tmp[,grep('DayWindSpdAvg', x)],
    precip = tmp[,grep('DayPrecip', x)])
  
  data <- as.data.frame(sapply(data, function(x) 
    as.numeric(as.character(x))))
  
  return(data)
}


get_station_info <- function(station_names){
  # Returns a comma separated list of station numbers
  # Given the station names
  
  stations <- getURL('http://et.water.ca.gov/api/station')
  tmp <- fromJSON(stations)
  i <- which(tmp$Stations$Name %in% station_names)
  lapply(i, function(j){
    station_nm <- tmp$Stations$Name[j]
    station_nbr <- tmp$Stations$StationNbr[j]
    station_lat <- tmp$Stations$HmsLatitude[j]
    station_long <- tmp$Stations$HmsLongitude[j]
    station_lat <- strsplit(station_lat, ' / ')[[1]][2]
    station_long <- strsplit(station_long, ' / ')[[1]][2]
    station_ele <- as.numeric(tmp$Stations$Elevation[j]) * 0.3048
    
    list(station_name = station_nm,
         station_nbr = station_nbr,
         station_lat = station_lat,
         station_long = station_long,
         station_ele = station_ele)
  })
}


make_station_idx <- function(station_info){
  # Creates a character string of station names
  # Given the list created by get_station_info
  paste(sapply(seq_along(station_info), function(i)
    station_info[[i]]$station_nbr), collapse = ",")
}

