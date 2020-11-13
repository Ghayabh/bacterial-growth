#' This is an expanded version of the Jameson model by Giménez and Dalgaard (2004),this new model includes a temperature dependent parameter (gamma).
#' @details
#' \deqn{\begin{equation}\begin{array}{l}
#'t<t_{\operatorname{lag}-S}, \quad \frac{\mathrm{d} S / \mathrm{d} t}{S_{t}}=0 \\
#'t \geq t_{\operatorname{lag}-S}, \quad \frac{\mathrm{d} S / \mathrm{d} t}{S_{t}}=\mu_{\max }^{S} \times\left(1-\frac{S_{t}}{S_{\max }}\right) \times\left(1-\frac{\gamma \times \mathrm{NB}_{t}}{\mathrm{NB}_{\max }}\right) \\
#'t<t_{\mathrm{lag}-\mathrm{NB}}, \quad \frac{\mathrm{d} \mathrm{NB} / \mathrm{d} t}{\mathrm{NB}}=0 \\
#'t \geq t_{\mathrm{lag}-\mathrm{NB}}, \quad \frac{\mathrm{d} \mathrm{NB} / \mathrm{dt}}{\mathrm{NB}}=\mu_{\max }^{\mathrm{NB}} \times\left(1-\frac{\mathrm{NB}_{t}}{\mathrm{NB}_{\max }}\right) \times\left(1-\frac{S_{t}}{S_{\operatorname{mox}}}\right)
#'\end{array}\end{equation}}
#' @import deSolve
#' @param t time vector
#' @param state initial values of Q1 (physiological state of the first flora),Q2 (physiological state of the second flora), y1 (the flora population 1) and y2 (the flora population 2) for the first time value.
#' @param parameters parameter list
#'
#' @return dQ1,dQ2,dy1,dy2
#' @export
#'
#' @examples
#' library(deSolve)
#'out <- ode(y = c(Q1=1/((1/exp(-2))-1),Q2=1/((1/exp(-10))-1),y1=1,y2=10),
#'times = seq(from=0, to=200, by = 1), func = expandedjameson,
#'parms = c(gamma=1, mumax1 = 0.14, mumax2=0.3, ymax1=100000,ymax2=10000000))
#'plot(out)
#'plot(time,log10(out[,"y1"]),col="red", lwd = 2, lty = 1)
#'plot(time,log10(out[,"y2"]),col="green", lwd = 2, lty = 1)
expandedjameson<-function(t,state,parameters){
  with(
    as.list(c(state,parameters)),{
      dQ1<-mumax1*Q1
      dQ2<-mumax2*Q2
      dy1<-(Q1/(1+Q1))*mumax1*(1-(y1/ymax1))*(1-(gamma*y2/ymax2))*y1
      dy2<-(Q2/(1+Q2))*mumax2*(1-(y1/ymax1))*(1-(y2/ymax2))*y2
      return(list(c(dQ1,dQ2,dy1,dy2)))
    }
  )
}
