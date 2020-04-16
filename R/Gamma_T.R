#' This secondary growth model describes the influence of temperature over mumax
#'
#' @param Tmin Minimal growth temperature #a number
#' @param Topt Optimal growth temperature #a number
#' @param Tmax Maximal growth temperature # a number
#' @param T Temperature # a number
#' @param muopt optimal growth rate # a number
#' @return sqrmumax^2= mumax #maximum growth rate # a number
#' @export
#'
#' @examples
#' Gamma_T(10,4,20,37,0.037)
Gamma_T <- function(T,Tmin, Topt, Tmax,muopt)
{sqrmumax<-sqrt(((T >= Tmin) & (T <= Tmax)) * muopt * (T - Tmax) * (T - Tmin)^2 / ((Topt - Tmin) * ((Topt - Tmin) * (T - Topt) - (Topt - Tmax) * (Topt + Tmin - 2.0 * T))))
return(sqrmumax^2)
}
