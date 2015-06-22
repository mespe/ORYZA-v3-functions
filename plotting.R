## Functions to plot ORYZA weather files
## M. Espe
## May 2015

src <- 'C:/Users/mespe/Documents/CA-Variety-Trial-Dataset/ORYZA_files/temp/'

src <- '~/Dropbox/UNL_ORYZA/USA_Rice/USA_weather/'

aa <- list.files(path = src,
           pattern = '*.[0-9]{3}', full.names = TRUE)

read_weather <- function(weather_file)
{
  ll <- readLines(weather_file)
  idx <- grep('^\\*', ll)
  tmp <- read.table(textConnection(ll), skip = length(idx)+1,
                    sep = ',',
                    stringsAsFactors = FALSE)

  colnames(tmp) <- c('stn', 'year', 'doy', 'srad',
                     'tmin', 'tmax', 'vp', 'wind', 'prec' )
  return(tmp)
}

replace_99 <- function(dd)
{
 as.data.frame(lapply(dd, function(x) {
  x[x == -99] <- NA
  x}))
}

tmp <- sapply(aa, read_weather, USE.NAMES = TRUE, simplify = FALSE)
tmp <- lapply(tmp, replace_99)

par(mfrow = c(3,3))
mains <- gsub(src, '',
              names(tmp))

for(i in seq_along(tmp)){
plot(tmp[[i]]$tmax, type = 'l', ylim=c(0,60),
     main = paste(i, ":", mains[i]))
}

get_coord <- function(weather_file)
{
  ll <- readLines(weather_file)
  idx <- grep('^\\*', ll)
  read.table(textConnection(ll), skip = length(idx),
                    nrow = 1,
                    sep = ',',
                    stringsAsFactors = FALSE)

}

stut <- grep('usar1', mains)
pwr <- grep('usar999', mains)
mains[stut]
mains[pwr]
yr_stut <- gsub('usar1.', '', mains[stut])
yr_pwr <- gsub('usar999.', '', mains[pwr])
match(yr_stut, yr_pwr)

par(mfrow = c(3,3))
for(i in stut[-1]){
  plot(tmp[[i]]$tmin, type = "l", main = mains[stut[i]])
  lines(tmp[[pwr[i-1]]]$tmin, col = 'red')
}
