## Format S. American data for ORYZA(v3) using R
## M. Espe
## June 2015

library(Roryza)
library(readxl)

file_loc <- '~/Downloads/TACUAREMBO.xlsx'

sheets <- excel_sheets(file_loc)

data_pos <- grep('^[0-9]', sheets)

tmp <- lapply(data_pos, function(i) read_excel(path = file_loc,
                                               sheet = i))
str(tmp)

head(tmp[[1]])

colnames(tmp[[1]])

regx <- c('^year$', '^day$', '^Solar', 'xima', 'nima', '^Wind', '^Vapor', '^Precip')

idx <- sapply(regx, function(x) grep(x, colnames(tmp[[2]])))
ll <- tmp[[2]][,idx]

colnames(ll) <- c('year', 'day', 'solrad', 'tmax', 'tmin', 'wind', 'vp', 'precip')
ll$year <- format(as.Date(ll$year, '%Y-%m-%d'), '%Y')
loc <- read_excel(file_loc, sheet = grep('^LOCAL', sheets))

pwr <- Roryza:::getPOWER(lat = loc$LATITUD, lon = loc$LONGITUD,
                         ys = unique(ll$year), ye = unique(ll$year))


na_pos <- apply(ll, 2, is.na)











  pwr_idx <- match(as.numeric(ll$day), pwr$weday)
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
    # Uses converstion formula from NOAA
    vappre = round((6.11 * 10^((7.5 * pwr$tdew)/(237.3 + pwr$tdew)))/10, 2),
    wind = pwr$wind,
    precip = pwr$rain)

  file_name <- paste0(prefix, station_nbr, '.', gsub('^[1|2]', '', year))
  ff <- file(file_name, 'w')
  on.exit(close(ff))
  writeLines(c('* LSU data formatted for ORYZA(v3)',
               '* Missing values filled in from NASA POWER'), ff)
  writeLines(paste(long, lat, 0,0,0, sep = ','), ff)
  write.table(tmp, ff, row.names = FALSE, col.names = FALSE,
              quote = FALSE,
              eol = '\r\n', sep =",")



