
#'This function describes the extended Lambert–Pearson model (ELPM), who model the effect of pH (as hydrogen and hydroxyl ions) over the whole biokinetic pH range.
#' @details
#' \deqn{\begin{equation}\begin{aligned}
#'\frac{\mathrm{d} \gamma}{\mathrm{dpH}}=& 2.303\left\{P_{2}\left(\frac{10^{-\mathrm{pH}}}{P_{1}}\right)^{P_{2}}-P_{4}\left(\frac{10^{\mathrm{pH}-14}}{P_{3}}\right)^{P_{4}}\right\} \\
#'& \exp \left(-\left(\frac{10^{-\mathrm{pH}}}{P_{1}}\right)^{P_{2}}+\left(\frac{10^{\mathrm{pH}-14}}{P_{3}}\right)^{P_{4}}\right)
#'\end{aligned}\end{equation}}
#' @param pH pH #a number
#' @param P1 a parameter of the ELPM model # a number
#' @param P2 a parameter of the ELPM model # a number
#' @param P3 a parameter of the ELPM model # a number
#' @param P4 a parameter of the ELPM model # a number
#' @param muopt Optimal growth rate # a number a number
#'
#' @return mumax #maximum growth rate # a number
#' @export
#'
#' @examples
#' modELPM( 7.585261, 0.000132, 0.889, 0.0004, 0.0000005, 0.5)
modELPM<-function(pH,P1,P2,P3,P4,muopt){
  mumax<-muopt*(2.303*(P2*(((10^(-pH))/P1)^P2)-(P4*(((10^(pH-14)/P3)^P4))))*exp((-((10^(-pH))/P1)^P2)+((10^(pH-14)/P3)^P4)))
  return(mumax)
}
