## Wrapper function for C code to calculate heat units
## M. Espe
## July 2015

heat_units <- function(tmin, tmax, tbase, topt, thigh)
    {
    .C('heat_units', n = as.integer(length(tmin)),
       t_min = as.numeric(tmin),
       t_max = as.numeric(tmax),
       ans = as.numeric(rep(0, length(tmin))),
       t_base = as.numeric(tbase),
       t_opt = as.numeric(topt),
       t_high = as.numeric(thigh))$ans

}
