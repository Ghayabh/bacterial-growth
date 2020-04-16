#' This function describes the evolution of the single-cell growth probability with water activity (aw). For this, we used the model proposed by Jean Christophe Augustin and Aurelia Czarnecka-Kwasiborski in 2012, to describe the increase of the probability from 0 to 1 with increasing values of aw.
#'
#' @param aw water activity value #a number
#' @param awinf The cellular minimal value of aw for growth # a number
#' @param awsup The cellular maximal value of aw for growth # a number
#'
#' @return proba_pH_Aug The single-cell growth probability # a number
#' @export
#'
#' @examples
#' Proba_aw(aw=0.95,awinf = 0.917,awsup = 0.988)the values of awinf & awsup were imported from Augustin's 2012 review
#' Proba_aw(0.90,awinf=0.917,awsup=0.988)
#' Proba_aw(0.99,awin=0.917,awsup=0.988)
Proba_aw<-function(aw,awinf,awsup){
  proba_aw_Aug<-c()
  for (i in 1:length(aw)) {

  }
  if (aw[i]<=awinf){
    proba_aw_Aug[i]=0
  }
  else
  {if(aw[i]>=awsup){
    proba_aw_Aug[i]=1
  }else
    proba_aw_Aug[i]<-(aw[i]-awinf)/(awsup-awinf)
  }

  return(proba_aw_Aug)
}

