## Write an Oryza weather file given a data.frame
## M. Espe
## June 2015


##' Generate file path for Oryza weather
##'
##' This is a convenience function to programatically generate the
##'    the file path required for \code{write_weather}
##' @title Generate weather file path
##' @param folder the folder where files will be saved
##' @param prefix the prefix for the Oryza weather file
##' @param dataframe The dataframe of weather data to be written to Oryza
##' @return the complete file path for the weather file, formatted for Oryza
##' @author Matthew Espe
gen_path <- function(folder, prefix, dataframe)
    {
    station <- dataframe[1,1]
    year <- dataframe[1,2]
    paste0(folder, prefix, station, '.',
           gsub('^.', '', as.character(year)))

}

##' Write Oryza weather
##'
##' This function takes a data.frame object and writes it to a new
##'     file in the format required by Oryza(v3)
##'
##' @title Write Oryza weather file
##' @param weather_data a data.frame object of weather data required for Oryza
##'    This data frame must be in the correct order for oryza
##' @param long longitude in decimal degrees
##' @param lat latitude in decimal degrees
##' @param ele elevation
##' @param header A header to include at the top of the file, if any
##' @param file_path Complete file path to location where file will be saved
##'     including the file name.  This can be generated
##' @return null
##' @author Matthew Espe
write_weather <- function(weather_data,
                          long, lat, ele = 0,
                          header, file_path)
{
    ff <- file(file_name, 'w')
    on.exit(close(ff))
    writeLines(header, ff)
    writeLines(paste(long, lat, ele, 0, 0, sep = ','), ff)
    write.table(weather_data, ff, row.names = FALSE, col.names = FALSE,
                quote = FALSE, sep = ",")

}

## Written from N. Guilpart's script following GYGA protocol
##' Write weather header for Oryza(v3)
##'
##' This function writes the header for an Oryza weather file
##' @title Write weather header
##' @param ... values to be filled into the header template,
##'     in the form of key = value pairs.  Keys should match
##'     the lines in the first __ lines of the header template.
##' @return a character vector with values filled in
##' @author Matthew Espe
write_header <- function(...)
    {
    dots <- list(...)
    header_lines <- readLines('~/oryza_functions/Roryza/R/header_template.txt')

    i <- sapply(seq_along(dots), function(i)
        grep(names(dots)[i], header_lines, ignore.case = TRUE)[1])
    for(j in i){
        header_lines[j] <- paste(header_lines[j], dots[j == i], sep = ' ')
    }

    header_lines[length(header_lines) + 1]
    return(header_lines)
}


## Tests

## write_header(country = 'usa',
##              station_num = 1,
##              station_name = 'colusa',
##              year = 2012, source = 'CIMIS',
##              author = 'me',
##              date = Sys.time(),
##              long = -90,
##              lat = 38,
##              ele = 0)
