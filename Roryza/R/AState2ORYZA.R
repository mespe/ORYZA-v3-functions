# getAState_weather
# M. Espe
# Apr 2015

getAState_weather <- function(StartDate, EndDate, 
                              Units = 'C', Station = "Judd Hill")
{
  
  # Returns data from AState weather database
  # StartDate = StartDate in format m/d/Y or m-d-Y
  # EndDate = Enddate in formate m/d/Y or m-d-Y
  # Units = C or F
  # Station = Judd Hill, Wildy, ASU, or Sullivan
  
  require(XML)
  require(RCurl)
  require(RHTMLForms)
  
  u = "http://weather.astate.edu/Reports.asp?"
  
  doc <- getForm(u,
                 Station = Station,
                 StartDate = StartDate,
                 EndDate = EndDate,
                 Units = Units)
  
  readHTMLTable(doc, stringsAsFactors = FALSE)[[2]]
}

AState2ORYZA <- function(year, Station = 'Jude Hill', station_nbr,
                         prefix = 'usar')
{
  # This function retrieves the data from ASU weather network and
  # processes it for direct use by ORYZA(v3)
  # Weather file is saved to CURRENT WORKING DIRECTORY
  #
  # year = year of data requested
  # station_nbr = user-defined value of station number
  # Station = station name (Judd Hill, Wildy, ASU, or Sullivan)
  # prefix = file name prefix for ORYZA weather file (e.g. - uscaXX.XXX)
  
  ASU_data <- getAState_weather(StartDate = paste0('1/1/', year),
                                EndDate = paste0('12/31/', year),
                                Station = Station)
  
  tmp <- data.frame(
    station_nbr = station_nbr,
    year = year,
    day = as.integer(format(as.Date(ASU_data[,1], '%m/%d/%Y'), '%j')),
    srad = as.numeric(ASU_data[,13]) * 1000,
    tmin = ASU_data[,5],
    tmax = ASU_data[,3],
    # Approximate vappres using sat vapor = 25
    vappre = round(as.numeric(ASU_data[,11]) * 0.25, 3),
    wind = ASU_data[,7],
    precip = ASU_data[,14],
    stringsAsFactors = FALSE)
  
  file_name <- paste0(prefix, station_nbr, '.', gsub('^[1|2]', '', year))
  ff <- file(file_name, 'w')
  on.exit(close(ff))
  writeLines('* ASU climate data formatted for ORYZA(v3)', ff)
  # Approximate location - fix later
  writeLines(paste(35.5, -90.5, 0,0,0, sep = ','), ff)
  write.table(tmp, ff, row.names = FALSE, col.names = FALSE,
              quote = FALSE, 
              eol = '\r\n', sep =",")
}

# Test function
getwd()
AState2ORYZA(year = 2013, station_nbr = 2, prefix = 'usar')
