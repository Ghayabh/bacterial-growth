#' This extended Presser model was quoted by Tienungoon et al. (2000) for the full biokinetic range
#'@details
#'\deqn{\begin{equation}\frac{\mu_{\max }}{\mu_{\mathrm{opt}}}=\gamma_{\mathrm{pH}}=\left(1-10^{\mathrm{pH}_{\min }-\mathrm{pH}}\right)\left(1-10^{\mathrm{pH}-\mathrm{pH}_{\max }}\right)\end{equation}}
#' @param pH pH # a number
#' @param pHmin Minimal growth pH #a number
#' @param pHmax Maximal growth pH #a number
#' @param muopt Optimal growth rate # a number
#'
#' @return sqrmumax^2= mumax #maximum growth rate # a number
#' @export
#'
#' @examples
#' modelPresserext(7,3,9,0.5)
#' modelPresserext(c(7,5),3,9,0.5)
modelPresserext<-function(pH,pHmin,pHmax,muopt){
  sqrtmumax<-c()
  for (i in 1:length(pH)){
    if (pH[i]<=pHmin) {
      sqrtmumax[i]<-0
    }
    else
      if(pH[i]>=pHmax){
        sqrtmumax[i]<-0
      }
    else{
      sqrtmumax[i]<-sqrt(muopt*(1-(10^(pHmin-pH[i])))*(1-(10^(pH[i]-pHmax))))
    }
  }
  return(sqrtmumax^2)
}
