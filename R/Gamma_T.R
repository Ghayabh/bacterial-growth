#' This secondary growth model describes the influence of temperature over mumax
#' @details
#' \deqn{\begin{equation}\begin{array}{l}
#'\text { "cpm_T" <- as.formula(sqrtmumax }^{\sim} \text { sqrt }\left(((T>=\operatorname{Tmin}) \&(T<=\operatorname{Tmax}))^{*} \text { muopt }^{*}(T-\operatorname{Tmax})^{*}(T-\operatorname{Tmin})^{\wedge} 2 /(\text { (Topt - } \operatorname{Tmin})^{*}\right. \\
#'\left.\left.\left((\text { Topt }-\operatorname{Tmin})^{*}(T-\text { Topt })-(\text { Topt }-\operatorname{Tmax})^{*}\left(\text { Topt }+\operatorname{Tmin}-2.0^{*} T\right)\right)\right)\right) \text { ) }
#'\end{array}\end{equation}}
#' @param Tmin Minimal growth temperature #a number
#' @param Topt Optimal growth temperature #a number
#' @param Tmax Maximal growth temperature # a number
#' @param T Temperature # a number
#' @param muopt optimal growth rate # a number
#' @return sqrmumax^2= mumax #maximum growth rate # a number
#' @export
#'
#' @examples
#' Gamma_T(10,4,37,20,0.037)
Gamma_T <- function(T,Tmin, Tmax, Topt,muopt)
{mumax<-((T >= Tmin) & (T <= Tmax)) * muopt * (T - Tmax) * (T - Tmin)^2 / ((Topt - Tmin) * ((Topt - Tmin) * (T - Topt) - (Topt - Tmax) * (Topt + Tmin - 2.0 * T)))
return(mumax)
}

