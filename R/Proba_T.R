#' This function describes the evolution of the single-cell growth probability with temperature. For this, we used the model proposed by Jean Christophe Augustin and Aurelia Czarnecka-Kwasiborski in 2012, to describe the increase of the probability from 0 to 1 with increasing values of temperature.
#'@details
#'\deqn{\begin{equation}p(T)=\left\{\begin{array}{ll}
#'0, & T \leq T_{\mathrm{inf}} \\
#'\frac{\exp (T / c)-\exp \left(T_{\mathrm{inf}} / c\right)}{\exp \left(T_{\mathrm{sup}} / c\right)-\exp \left(T_{\mathrm{inf} / c}\right)}, & T_{\mathrm{inf}}<T<T_{\mathrm{sup}} \\
#'1, & T \geq T_{\mathrm{sup}}
#'\end{array}\right.\end{equation}}
#'
#' @param T Temperature # a number
#' @param Tinf The theoritical cellular minimal temperature for growth #a number
#' @param Tsup The theoritical cellular maximal temperature for growth #a number
#' @param c a parameter #a number

#' @return proba_T_Aug The single-cell growth probability, which increases from O to 1 when temperature value increases at a more favorable condition #a number
#' @export
#'
#' @examples
#' Proba_T(T=10,Tinf=-3.6,Tsup=17.3,c=7.6) the values of Tinf,Tsup & c were imported from Augustin's 2012 review
#' Proba_T(T=20,Tinf=-3.6,Tsup=17.3,c=7.6)
Proba_T<- function(T,Tinf,Tsup,c) {
  proba_T_Aug<-c()

  for (i in 1:length(T)) {

  if (T[i]<=Tinf){
    proba_T_Aug[i]<-0
  }
  else
  {if(T[i]>=Tsup){
    proba_T_Aug[i]<-1
  }
    else
      proba_T_Aug[i]<-(exp(T[i]/c)-exp(Tinf/c))/(exp(Tsup/c)-exp(Tinf/c))
  }}
  return(proba_T_Aug)
}

