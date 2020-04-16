#' The logistic primary growth model using delay and rupture, developped by Rosso in 1996, describes growth as a function of time.
#'
#' @param temp time (h)
#' @param mumax maximum growth rate # a number
#' @param lag lag time # a number
#' @param Nmax maximum population size #a number
#' @param N0 initial population size # a number
#'
#' @return log10N #a number
#' @export
#'
#' @examples
#' modrosso(temp=seq(0,24,0.5),mumax=0.37,lag=4,Nmax = 10^5,N0=100)
modrosso<-function(temp,mumax,lag,Nmax,N0){log10N<- (temp<=lag)*log10(N0)+(temp>lag)*
  (log10(Nmax)-log10(1+((Nmax/N0)-1)*exp(-mumax*(temp-lag))))
return(log10N)
}
