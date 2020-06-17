#' The differential form of the predictive growth model developed by Baranyi & Roberts in 1994, to predict microbial growth in food environments.
#' @details
#' \deqn{\begin{equation}d Q=\operatorname{mu_{max}}^{*} Q\end{equation}}
#' \deqn{\begin{equation}d y=(Q /(1+Q))^{*} \operatorname{mu_{max}}^{*}(1-(y / y_{max} ))^{*} y\end{equation}}
#' @param t time vector
#' @param state initial values of y and Q for the first time value.
#' @param parameters parameter list
#' @import deSolve
#' @importFrom deSolve ode
#' @return dQ, dy
#' @export
#'
#' @examples
#'library(deSolve)
#'K=9.21
#'Q=1/((1/exp(-K))-1)
#'Q
#'state <- c(Q=Q,y=15)
#'time <- seq(from=0, to=70, by = 1)
#'parameters <- c(mumax = 1, ymax=100000000)
#'out_baranyi <- ode(y = state, times = time, func = baranyi_diff, parms = parameters)
#'plot(time,log10(out_baranyi[,"y"]),lty=1,col='red')
baranyi_diff<-function(t,state,parameters){
  with(
    as.list(c(state,parameters)),{
      dQ<-mumax*Q
      dy<-(Q/(1+Q))*mumax*(1-(y/ymax))*y
      return(list(c(dQ,dy)))
    }
  )
}


