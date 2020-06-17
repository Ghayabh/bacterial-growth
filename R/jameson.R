#' This model proposed by Giménez and Dalgaard (2004). It corresponds to the Jameson effect and predicts that one microorganism stops growing when the other has reached its maximum population density (MPD).
#' @details
#' \deqn{\begin{equation}\begin{aligned}
#'&\frac{d N_{L m}}{d t}=\alpha_{L m}(t) \cdot \mu_{\max }^{L m} \cdot\left(1-\frac{N_{L m}}{N_{\max }^{L M}}\right) \cdot\left(1-\frac{N_{L A B}}{N_{\max }^{L 4 B}}\right)\\
#'&\alpha_{L m}(t)=\frac{q_{L m}(t)}{1+q_{L m}(t)} ; \quad \frac{d q_{L m}}{d t}=\mu_{\max }^{L m} \cdot q_{L m}(t) ; \quad N_{L m}(0)=N_{L m 0}\\
#'&\frac{d N_{L A B}}{d t}=\alpha_{L A B}(t) \cdot \mu_{\max }^{L A B} \cdot\left(1-\frac{N_{L A B}}{N_{\max }^{L A B}}\right) \cdot\left(1-\frac{N_{L_{m}}}{N_{\max }^{L_{m}}}\right)\\
#'&\alpha_{L A B}(t)=\frac{q_{L A B}(t)}{1+q_{h, B}(t)} ; \quad \frac{d q_{A B}}{d t}=\mu_{\max }^{L A B} \cdot q_{L A B}(t), \quad N_{L A B}(0)=N_{L A B 0}
#'\end{aligned}\end{equation}}
#' @import deSolve
#' @param t time vector
#' @param state initial values of Q1 (physiological state of the first flora),Q2 (physiological state of the second flora), y1 (the flora population 1) and y2 (the flora population 2) for the first time value.
#' @param parameters parameter list
#'
#' @return dQ1,dQ2,dy1,dy2
#' @export
#'
#' @examples
#'library(deSolve)
#'K2=10
#'Q2=1/((1/exp(-K2))-1)
#'K1=2
#'Q1=1/((1/exp(-K1))-1)
#'state <- c(Q1=Q1,Q2=Q2,y1=1,y2=10)
#'time <- seq(from=0, to=200, by = 1)
#'parameters <- c(mumax1 = 0.14, mumax2=0.3, ymax1=100000,ymax2=10000000)
#'out <- ode(y = c(Q1=1/((1/exp(-2))-1),Q2=1/((1/exp(-10))-1),y1=1,y2=10),times = seq(from=0, to=200, by = 1), func = jameson, parms = c(mumax1 = 0.14, mumax2=0.3, ymax1=100000,ymax2=10000000))
#'plot(out)
#'plot(time,log10(out[,"y1"]),col="red", lwd = 2, lty = 1)
#'plot(time,log10(out[,"y2"]),col="green", lwd = 2, lty = 1)
jameson<-function(t,state,parameters){
  with(
    as.list(c(state,parameters)),{
      dQ1<-mumax1*Q1
      dQ2<-mumax2*Q2
      dy1<-(Q1/(1+Q1))*mumax1*(1-(y1/ymax1))*(1-(y2/ymax2))*y1
      dy2<-(Q2/(1+Q2))*mumax2*(1-(y1/ymax1))*(1-(y2/ymax2))*y2
      return(list(c(dQ1,dQ2,dy1,dy2)))
    }
  )
}




