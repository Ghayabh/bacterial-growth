#' this function of secondary growth model describe the evolution of the square root of the maximum specific growth rate (sqrtmumax) as a function of pH, This is a symetric cardinal pH model developped by Rosso & al.in 1995  with three parameters (pHmin, pHopt, muopt), obtained by fixing pHmax =2
#' @details
#'\deqn{\begin{equation}\begin{array}{l}
#'\mathrm{cpm}_{-} \mathrm{pH}_{-} 3 \mathrm{p}<-\text { as.formula }\left(\text { sqrtmumax } \sim \operatorname{sqrt}\left(\left((\mathrm{pH}>=\mathrm{p} \mathrm{Hmin}) \&\left(\mathrm{pH}<=\left(2^{*} \mathrm{pHopt}-\mathrm{pHmin}\right)\right)\right)\right)\right) \\
#'* \text { muopt }^{*}(\mathrm{pH}-\mathrm{pHmin})^{*}\left(\mathrm{pH}-\left(2^{*} \mathrm{pHopt}-\mathrm{pHmin}\right)\right) /\left((\mathrm{pH}-\mathrm{pHmin})^{*}(\mathrm{pH}-\right. \\                                                                                                                                                                       \left.\left.\left.\left.\left(2^{*} \mathrm{pHopt}-\mathrm{pHmin}\right)\right)-(\mathrm{pH}-\mathrm{pHopt})^{\wedge} 2\right)\right)\right)
#'\end{array}\end{equation}}
#' @param pH # a number
#' @param pHmin Minimal growth pH #a number
#' @param pHopt Optimal growth pH #a number
#' @param muopt Optimal growth rate # a number
#' @return sqrmumax^2= mumax #maximum growth rate # a number
#' @export
#'
#' @examples
#' Gamma_pH_3p(5,4,7,0.5)
#' Gamma_pH_3p(4,4,7,0.2)
#' Gamma_pH_3p(7,4,7,2)
Gamma_pH_3p <- function(pH,pHmin,pHopt,muopt)
{sqrtmumax<-sqrt(((pH >= pHmin) & (pH <=(2 * pHopt- pHmin))) * muopt
                   * (pH - pHmin) * (pH - ((2 * pHopt) - pHmin)) / ((pH - pHmin) * (pH - ((2 * pHopt) - pHmin)) - (pH - pHopt)^2))
return((sqrtmumax^2))
}

