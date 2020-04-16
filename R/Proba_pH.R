#' This function describes the evolution of the single-cell growth probability with pH. For this, we used the model proposed by Jean Christophe Augustin and Aurelia Czarnecka-Kwasiborski in 2012, to describe the increase of the probability from 0 to 1 with increasing values of pH.
#'
#' @param pH pH value # a number
#' @param pHinf The cellular minimal pH for growth # a number
#' @param pHsup The cellular maximal pH for growth # a number
#'
#' @return proba_pH_Aug The single-cell growth probability # a number
#' @export
#'
#' @examples
#' Proba_pH(5,pHinf=-4.34,pHsup=5.93) the values of phinf & phsup were imported from Augustin's 2012 review
#' Proba_pH(4,pHinf=-4.34,pHsup=5.93)
#' Proba_pH(7,pHinf=-4.34,pHsup=5.93)
Proba_pH<-function(pH,pHinf,pHsup){
  proba_pH_Aug<-c()
  for (i in 1:length(pH)) {

  }
  if(pH[i]<=pHinf){
    proba_pH_Aug[i]=0
  }

  else
  {if(pH[i]>=pHsup){
    proba_pH_Aug[i]=1
  } else
    proba_pH_Aug[i]<-  (exp(-pH[i])-exp(-pHinf))/(exp(-pHsup)-exp(-pHinf))

  }
  return(proba_pH_Aug)
}
