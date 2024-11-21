#Function for calculating water-year DOY. This will help facilitate plotting and analysizing trends in ice-in since they span either side of the winter-year (e.g., 2011-2012). For example, an IceInDayofYear_fed value of 150 means Ice-In occured 150 days after the start of the water-year (Oct1)

hydro.day = function(x, start.month = 10L) {
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}



loch_ws_size_m2 <- 6.6e+6


##########################################################################################
##Functions for calculating sens slopes and intercepts for plotting later
##########################################################################################
map_sens <- function(df) {
  sens.slope(df$value)
}


sens_slope <- function(mod) {
  mod$estimate[[1]]
}

#https://kevintshoemaker.github.io/NRES-746/TimeSeries_all.html
#For getting sens intercept, helpful for plotting later
map_zyp <- function(df) {
  zyp::zyp.sen(value ~ waterYear, df)
  # sens.slope(df$mean)
}


sens_intercept <- function(mod) {
  mod$coefficients[[1]] # pull out y-int estimate for ploting
}

