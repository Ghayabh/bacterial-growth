#' this function of secondary growth model describe the evolution of the square root of the maximum specific growth rate (sqrtmumax) as a function of water activity (aw), This is a symetric cardinal pH model developped by Rosso & Robinson in 2001 with two parameters (awmin, muopt) and awopt =1
#'
#' @param aw water activity #a number
#' @param awmin minimal growth water activity # a number
#' @param muopt Optimal growth rate # a number # a number
#'
#' @return sqrmumax^2= mumax #maximum growth rate # a number
#' @export
#'
#' @examples
#' Gamma_aw_2p(0.92,0.90,0.097)
Gamma_aw_2p <- function(aw,awmin,muopt)
{sqrtmumax <- sqrt((aw >= awmin) * muopt * (aw - awmin)^2 / (1 - awmin)^2)
return((sqrtmumax^2))
}
