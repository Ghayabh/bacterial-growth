#' this function of secondary growth model describe the evolution of the square root of the maximum specific growth rate (sqrtmumax) as a function of pH, This is a symetric cardinal pH model developped by Rosso & al.in 1995  with four parameters (pHmin, pHopt, pHmax,muopt)
#'
#' @param pH pH #a number
#' @param pHmin minimal groMh pH #a number
#' @param pHmax Maximal growth pH #a number
#' @param muopt Optimal growth rate # a number
#' @param pHopt Optimal growth pH #a number
#'
#' @return sqrmumax^2= mumax #maximum growth rate # a number
#' @export
#'
#' @examples
#' Gamma_pH_4p(5,4,9,0.5,7)
Gamma_pH_4p<- function(pH,pHmin,pHmax,muopt,pHopt)
{sqrtmumax<-sqrt(((pH >= pHmin) & (pH <= pHmax)) * muopt
                 *(pH - pHmin) * (pH - pHmax) / ((pH - pHmin) * (pH - pHmax) - (pH - pHopt)^2))
return((sqrtmumax^2))
}

