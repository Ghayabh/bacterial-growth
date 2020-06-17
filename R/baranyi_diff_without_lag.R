library(deSolve)
#' The differential form of the predictive growth model developed by Baranyi & Roberts in 1994, to predict microbial growth in food environments.
#' @details
#' \deqn{\begin{equation}d y=\operatorname{mu_{max}}^{*}(1-(y / y_{max} ))^{*} y\end{equation}}
#' @import deSolve
#' @param t time vector
#' @param state initial values of y  for the first time value.
#' @param parameters parameter list
#'
#' @return dy
#' @export
#'
#' @examples
#'library (deSolve)
#'state<-c(y=15)
#'time<-seq(0,70, 1)
#'parameters<- c(mumax=1, ymax=10000)
#'out_baranyi_without_lag<-ode(y = state, times = time, func = baranyi_diff_without_lag, parms = parameters)
#'plot(time,log10(out_baranyi_without_lag[,"y"]),lty=1,col='red')

baranyi_diff_without_lag<-function(t,state,parameters){
  with(
    as.list(c(state,parameters)),{
      dy<-mumax*(1-(y/ymax))*y
      return(list(c(dy)))
    }
  )
}


