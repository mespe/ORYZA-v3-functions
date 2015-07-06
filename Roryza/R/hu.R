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


## Create R based function for comparison

hu_slow <- function(tmin, tmax, tbase, topt, thigh)
{
  n <- length(tmin)  
  ans <- rep(0, n)
  
  for(day in seq(length.out = n)){
    for(hour in 1:24){
      tmp <- 
        ((tmax[day] + tmin[day])/2) + (tmax[day] - tmin[day]) * (cos(0.2618 * (hour - 14)) / 2)  
      print(tmp)
      if(tmp > tbase & tmp <= topt)
        ans[day] = ans[day] + (tmp - tbase)/24
      if(tmp > topt & tmp < thigh)
        ans[day] = ans[day] + (topt - (tmp - topt) * ((topt - tbase)/(thigh - topt)))/24
    }
  }
  return(ans) 
}