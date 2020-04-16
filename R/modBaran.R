#' The primary predictive model developped by Baranyi & Roberts in 1994, to predict microbial behavior in food environments.
#'
#' @param temp time(h)
#' @param mumax maximum growth rate # a number
#' @param lag lag time # a number
#' @param log10N0 Initial population density #a number
#' @param log10Nmax maximum population density # a number
#'
#' @return log10N #a number
#' @export
#'
#' @examples
#' modBaran(temp=seq(0,24,0.5), mumax = 0.037, lag=4, log10N0 = 1, log10Nmax = 5 )
modBaran<- function(temp,mumax,lag,log10N0,log10Nmax){
  log10N<-(log10Nmax-log10(1+((10^(log10Nmax-log10N0))-1)/(1-exp(-mumax*lag)+exp(-mumax*(lag-temp)))))
  return(log10N)
}
