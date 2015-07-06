## Algorithms for dealing wit missing data in weather files
## M. Espe
## June 2015

# ## TESTS
# library(zoo)
# dataframe <- read.csv(file = '~/Dropbox/UNL_ORYZA/USA_Rice/USA_weather/usar1.007',
#                       skip = 2, header = FALSE)
# line1 <- read.csv(file = '~/Dropbox/UNL_ORYZA/USA_Rice/USA_weather/usar1.007',
#                 nrow = 1, header = FALSE, comment = '*')
# 
# dataframe <- dataframe[-c(30:34, 100:120),]
# dataframe$V4[20:22] <- NA
# dataframe$V5[80:95] <- NA
# str(dataframe)
# lat = line1[1,2]
# lon = line1[1,1]
# year = 2007


interp_weather <- function(dataframe, log_path)
{
    ## Check for continuous data
    cont <- dataframe[1, 3]:dataframe[nrow(dataframe), 3]
    i <- match(cont, dataframe[,3])

    ## Record within missing for later
    in_col <- lapply(dataframe, function(x) which(is.na(x)))

    ## Linear interpolate missing data if less than 10 days
    if(any(is.na(i))){
        dataframe <- as.data.frame(
            sapply(dataframe, function(y)
                round(na.approx(x = dataframe[,3],
                                object = y, xout = cont,
                                maxgap = 10), 2))
            )
    }

    if(any(is.na(dataframe)))
        warning('Values still missing - fill in from other source')

    write_weather_log(log_path, dataframe, cont, i, in_col)

    return(dataframe)
}

## Write log file

write_weather_log <- function(file_path, dataframe, cont, i, in_col)
{
    ff <- file(file_path, 'w')
    on.exit(close(ff))

    writeLines(c('Spline interpolated weather data',
                 'Missing values for ___, processed:',
                 format(Sys.time(), '%Y-%m-%d'),
                 '\n',
                 'Positions missing contiguous:'), ff)
    writeLines(c(as.character(cont[is.na(i)]), '\n'), ff)
    writeLines('Positions missing in column:', ff)
    for(j in 4:ncol(dataframe)){
        writeLines(paste0('Column: ', j), ff)
        writeLines(as.character(in_col[[j]]), ff)
    }
    writeLines('Positions still missing:', ff)
    for(j in 4:ncol(dataframe)){
        writeLines(paste0('Column: ', j), ff)
        writeLines(as.character(which(is.na(dataframe[,j]))), ff)
    }

}

## Fill from NASA POWER with correction factor

replace_wPOWER <- function(dataframe, lat, lon, year, log_path)
{
    pwr <- POWER2ORYZA(station_nbr = 1, lat = lat, long = lon, year = year)

    ## Get corrected values between POWER and OBS
    idx <- match(dataframe[,3], pwr[,3], nomatch = 0)

    new_vals <- as.data.frame(sapply(seq_along(pwr), function(i){
      if(!all(is.na(dataframe[,i]))){
                           m <- lm(dataframe[,i] ~ pwr[idx,i])
                           round(coef(m)[1] + coef(m)[2] * pwr[,i], 2)
      }else{
        pwr[,i]
      }
                        }))

    ## Check for continuous data
    cont <- dataframe[1, 3]:dataframe[nrow(dataframe), 3]
    miss_days <- !(cont %in% dataframe[,3])

    ## Record the day in column missing
    in_col <- sapply(dataframe, function(x)
        dataframe[,3][is.na(x)])

    ## If missing, fill in first by linear interpolation, then replace with
    ## Corrected pwr data
    if(any(miss_days)){
        dataframe <- as.data.frame(lapply(dataframe, function(y){
          if(!all(is.na(y))){
            na.approx(
                x = dataframe[,3],
                object = y, xout = cont)
            }else{
              NA
            }
          }))
        dataframe[miss_days, c(4:9)] <- new_vals[cont[miss_days], c(4:9)]
    }

    ## Now fill in values that were/are missing in column
    ## Taking advantage of parallelism between pwr and new_vals
    for(i in seq_along(in_col)){
        dataframe[dataframe[,3] %in% in_col[[i]], i] <-
            new_vals[pwr[,3] %in% in_col[[i]], i]
    }
    ## Write log
    write_weather_log(log_path, dataframe, cont, miss_days, in_col)

    return(dataframe)
}
