#'  This is the primary predictive model developped by Baranyi & Roberts in 1994, to predict microbial behavior in food environments.The model modBaran_without_lag has four parameters (temps, mumax, log10N0, log10Nmax) without Nmax
#' @details
#'\deqn{\begin{equation}\begin{array}{l}
#'\mathrm{LOG} 10 \mathrm{N}=\mathrm{LOG} 10 \mathrm{N} 0+\operatorname{mumax}^{*} \mathrm{t} / \log (10)+\log 10\left(\exp \left(-\operatorname{mumax}^{*} \mathrm{t}\right) *\right. \\
#'\left.\left(1-\exp \left(-\operatorname{mumax}^{*} \mathrm{Lag}\right)\right)+\exp \left(-\operatorname{mumax}^{*} \mathrm{Lag}\right)\right)
#'\end{array}\end{equation}}
#' @param temp time (h)
#' @param mumax maximum growth rate # a number
#' @param lag lag time # a number
#' @param log10N0 Initial population density #a number
#'
#' @return log10N #a number
#' @export
#'
#' @examples
#'  modBaran_without_Nmax(seq(0,48,0.5), 0.035, 4, 3)
modBaran_without_Nmax<-function(temp,mumax,lag,log10N0){
  log10N <- log10N0 + mumax * temp/log(10) + log10(exp(-mumax * temp) * (1 - exp(-mumax * lag)) + exp(-mumax * lag))
return(log10N)
}

