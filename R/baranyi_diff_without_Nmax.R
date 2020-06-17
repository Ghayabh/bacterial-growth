library(deSolve)
#' The differential form of the predictive growth model developed by Baranyi & Roberts in 1994, to predict microbial growth in food environments.
#' @details
#' \deqn{\begin{equation}\mathrm{d} \mathrm{Q}=\mathrm{mu_{max}}^{*} \mathrm{Q}\end{equation}}
#' \deqn{\begin{equation}d y=(Q /(1+Q))^{*} \operatorname{mu_{max}}^{*} y\end{equation}}
#' @import deSolve
#' @param t time vector
#' @param state initial values of y and Q for the first time value.
#' @param parameters parameter list
#'
#' @return dQ, dy
#' @export
#'
#' @examples
#'library(deSolve)
#'K=9.21
#'Q=1/((1/exp(-K))-1)
#'state<-c(Q=Q,y=15)
#'time<-seq(0,70, 1)
#'parameters<- c(mumax=1)
#'out_baranyi_without_lag<-ode(y = state, times = time, func = baranyi_diff_without_Nmax, parms = parameters)
#'plot(time,log10(out_baranyi_without_lag[,"y"]),lty=1,col='red')
baranyi_diff_without_Nmax<-function(t,state,parameters){
  with(
    as.list(c(state,parameters)),{
      dQ<-mumax*Q
      dy<-(Q/(1+Q))*mumax*y
      return(list(c(dQ,dy)))
    }
  )
}

