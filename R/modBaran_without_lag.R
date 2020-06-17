#' This is the primary predictive model developped by Baranyi & Roberts in 1994, to predict microbial behavior in food environments.The model modBaran_without_lag has four parameters (temps, mumax, log10N0, log10Nmax) without lag phase.
#'@details
#'\deqn{\begin{equation}\begin{array}{l}
#'\mathrm{LOG} 10 \mathrm{N}=\left(\mathrm{LOG} 10 \mathrm{Nmax}-\log 10\left(1+\left(10_{\mathrm{m}}^{\wedge}(\mathrm{LOG} 10 \mathrm{Nmax}-\mathrm{LOG} 10 \mathrm{N} 0)-1\right)\right.\right. \\
#'\left.\left.* \exp \left(-\operatorname{mumax}^{*} \mathrm{t}\right)\right)\right)
#'\end{array}\end{equation}}
#' @param temp time(h)
#' @param mumax maximum growth rate # a number
#' @param log10N0 Initial population density #a number
#' @param log10Nmax maximum population density # a number
#'
#' @return log10N #a number
#' @export
#'
#' @examples
#' modBaran_without_lag(seq(0,24,1), 0.5,3,5)
modBaran_without_lag<-function(temp,mumax,log10N0,log10Nmax){
log10N <- (log10Nmax - log10(1 + (10^(log10Nmax - log10N0) - 1) * exp(-mumax * temp)))
return(log10N)
}
