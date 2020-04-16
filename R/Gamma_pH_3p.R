#' this function of secondary growth model describe the evolution of the square root of the maximum specific growth rate (sqrtmumax) as a function of pH, This is a symetric cardinal pH model developped by Rosso & al.in 1995  with three parameters (pHmin, pHopt, muopt), obtained by fixing pHmax =2
#'
#' @param pH # a number
#' @param pHmin Minimal growth pH #a number
#' @param pHopt Optimal growth pH #a number
#' @param muopt Optimal growth rate # a number
#' @return sqrmumax^2= mumax #maximum growth rate # a number
#' @export
#'
#' @examples
Gamma_pH_3p <- function(pH,pHmin,muopt,pHopt)
{sqrtmumax<-sqrt(((pH >= pHmin) & (pH <=(2 * pHopt- pHmin)) * muopt
                   * (pH - pHmin) * (pH - ((2 * pHopt) - pHmin)) / ((pH - pHmin) * (pH - ((2 * pHopt) - pHmin)) - (pH - pHopt)^2)))
return((sqrtmumax^2))
}


