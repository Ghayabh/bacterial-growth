#' The primary predictive model developped by Buchanan in 1918, to predict microbial behavior in food environments.
#' @details
#' \deqn{\begin{equation}\operatorname{Ln}[N(t)]=\ln \left(N_{0}\right) \quad \text { si } t \leq \lambda\end{equation}}
#' \deqn{\begin{equation}
#' \operatorname{Ln}[N(t)]=\ln \left(N_{0}\right)-\mu(t-\lambda) \quad \text { si } t>\lambda
#' \end{e\begin{equation}
#'\operatorname{Ln}[N(t)]=\ln \left(N_{\max }\right) \quad \operatorname{si} N(t) \geq N_{\max }
#'\end{equation}quation}}
#' \deqn{}
#'
#' @param temp time vector
#' @param mumax maximum growth rate # a number
#' @param lag lag time # a number
#' @param log10N0 Initial population density #a number
#' @param log10Nmax maximum population density # a number
#'
#' @return LOG10N
#' @export
#'
#' @examples
#' temp<-seq(0,48,0.5)
#' modBuchanan(temp,1,9,0.5,4)
modBuchanan<-function(temp,log10N0,log10Nmax,mumax,lag){
  log10N<-log10N0 + (temp >= lag) * (temp <= (lag + (log10Nmax - log10N0) * log(10) / mumax)) *
    mumax * (temp - lag) / log(10) + (temp >= lag) * (temp>(lag + (log10Nmax - log10N0) * log(10) / mumax)) * (log10Nmax - log10N0)
  return(log10N)
}
