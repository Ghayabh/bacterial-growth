
#' This function describes the evolution of the probability of single-cell growth with environmental factors (T, pH & aw). For this, we used the model proposed by Jean Christophe Augustin and Aurelia Czarnecka-Kwasiborski in 2012, to describe the combined effect of the three environmental factors. in this model T, pH and aw were considered to have independent effects on the single cell
#'@details
#'\deqn{\begin{equation}p\left(T, \mathrm{pH}, a_{w}\right)=p(T) \cdot p(\mathrm{pH}) \cdot p\left(\mathrm{a}_{\mathrm{w}}\right)\end{equation}}
#'
#' @param T  Temperature value # a number
#' @param Tinf The theoritical cellular minimal temperature for growth #a number
#' @param Tsup The theoritical cellular maximal temperature for growth #a number
#' @param c Parameter # a number
#' @param pH pH value # a number
#' @param pHinf The cellular minimal pH for growth # a number
#' @param pHsup The cellular maximal pH for growth # a number
#' @param aw water activity value #a number
#' @param awinf The cellular minimal value of aw for growth # a number
#' @param awsup The cellular maximal value of aw for growth # a number
#'
#' @return Pc The single-cell growth probability as function of environemental factors # a number
#' @export
#'
#' @examples
#' Proba_c(10,Tinf=-3.6,Tsup=17.3,5,c=7.6,pHinf=-4.34,pHsup=5.93,0.95,awinf=0.917,awsup=0.988)
Proba_c <- function(T,Tinf,Tsup,c,pH,pHinf,pHsup,aw,awinf,awsup){
  Pc<-(Proba_T(T,Tinf,Tsup,c)*Proba_pH(pH,pHinf,pHsup)*Proba_aw(aw,awinf,awsup))
  return(Pc)
}


