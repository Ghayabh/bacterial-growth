#' this function of secondary growth model describe the evolution of the square root of the maximum specific growth rate (sqrtmumax) as a function of pH, This is a symetric cardinal pH model developped by Rosso & al.in 1995  with four parameters (pHmin, pHopt, pHmax,muopt)
#' @details
#'\deqn{\begin{equation}\begin{aligned}
#'&\text { "cpm_pH_4p" <- as.formula(sqrtmumax }^{\sim} \text { sqrt }\left(((\mathrm{pH}>\text { > } \text { pHmin }) \&(\mathrm{pH}<\mathrm{pHmax})) \text { * muopt } *(\mathrm{pH}-\mathrm{pHmin})^{*}(\mathrm{pH}-\mathrm{pHmax}) / \mathrm{(pH}\right.\\
#'&\text { pHmin) }\left.^{*}(\mathrm{pH}-\mathrm{pHmax})-(\mathrm{pH}-\mathrm{pHopt})^{\wedge} 2\right)
#'\end{aligned}\end{equation}}
#' @param pH pH #a number
#' @param pHmin minimal groMh pH #a number
#' @param pHmax Maximal growth pH #a number
#' @param muopt Optimal growth rate # a number
#' @param pHopt Optimal growth pH #a number
#'
#' @return mumax #maximum growth rate # a number
#' @export
#'
#' @examples
#' Gamma_pH_4p(5,4,9,7,0.5)
Gamma_pH_4p<- function(pH,pHmin,pHmax,pHopt,muopt)
{mumax<-(((pH >= pHmin) & (pH <= pHmax)) * muopt
                 *(pH - pHmin) * (pH - pHmax) / ((pH - pHmin) * (pH - pHmax) - (pH - pHopt)^2))
return(mumax)
}

