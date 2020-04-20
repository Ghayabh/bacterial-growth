#' this function of secondary growth model describe the evolution of the square root of the maximum specific growth rate (sqrtmumax) as a function of water activity (aw), This is a symetric cardinal pH model developped by Rosso & Robinson in 2001 with three parameters (awmin, awopt, muopt)
#' @param aw water activity #a number
#' @param awmin minimal growth water activity # a number
#' @param awopt optimal growth water activity # a number
#' @param muopt Optimal growth rate # a number # a number
#'
#' @return sqrmumax^2= mumax #maximum growth rate # a number
#' @export
#'
#' @examples
#' Gamma_aw_3p(0.95,0.90, 0.98, 0.5)
Gamma_aw_3p<-function(aw,awmin,awopt,muopt)
{ sqrtmumax <- sqrt((aw >= awmin) * muopt * (aw - 1)*(aw - awmin)^2 / ((awopt - awmin) * ((awopt - awmin) * (aw - awopt) - (awopt - 1) * (awopt + awmin - 2 * aw))))
return((sqrtmumax ^ 2))
}

