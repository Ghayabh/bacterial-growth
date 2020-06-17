#' This model is developed by  Zwietering et al. (1992, 1993).It's a square root type model but with an extra ﬁtting parameter-c2
#'@details
#'\deqn{\begin{equation}\gamma_{\mathrm{pH}}=\left[\frac{\left(\mathrm{pH}-\mathrm{pH}_{\min }\right)\left(1-\exp \left\{\mathrm{c}_{2}\left(\mathrm{pH}-\mathrm{pH}_{\max }\right)\right\}\right)}{\left(\mathrm{pH}_{\mathrm{opt}}-\mathrm{pH}_{\min }\right)\left(1-\exp \left\{\mathrm{c}_{2}\left(\mathrm{pH}_{\mathrm{opt}}-\mathrm{pH}_{\max }\right)\right\}\right)}\right]^{2}\end{equation}}
#' @param pH pH # a number
#' @param pHmin Minimal growth pH #a number
#' @param pHmax Maximal growth pH #a number
#' @param pHopt Optimal growth pH #a number
#' @param muopt Optimal growth rate # a number
#' @param c2 a parameter # a number
#'
#' @return sqrmumax^2= mumax #maximum growth rate # a number
#' @export
#'
#' @examples
#' modZwite(5,3,9,7,0.5,0.4)
#' modZwite(c(5,7),3,9,7,0.5,0.4)
modZwite<-function(pH,pHmin,pHmax,pHopt,muopt,c2){
  sqrtmumax<-c()
  for (i in 1:length(pH)) {
    if (pH[i]<=pHmin) {
      sqrtmumax[i]<-0
    }
    else
      if (pH[i]>=pHmax){
        sqrtmumax[i]<-0
      }
    else
      sqrtmumax[i]<-sqrt(((pH[i]-pHmin)*(1-(exp(c2*(pH[i]-pHmax)))))/ ((pHopt-pHmin)*(1-(exp(c2*(pHopt-pHmax)))))*muopt)
  }
  return(sqrtmumax^2)
}
