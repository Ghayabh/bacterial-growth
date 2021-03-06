% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modPresser.R
\name{modPresser}
\alias{modPresser}
\title{The following function is suggested by Presser et al. (1997) for the suboptimal pH range.}
\usage{
modPresser(pH, pHmin, muopt)
}
\arguments{
\item{pH}{pH # a number}

\item{pHmin}{pHmin Minimal growth pH #a number}

\item{muopt}{Optimal growth rate # a number}
}
\value{
mumax #maximum growth rate # a number
}
\description{
The following function is suggested by Presser et al. (1997) for the suboptimal pH range.
}
\details{
\deqn{\begin{equation}\frac{\mu_{\max }}{\mu_{\mathrm{opt}}}=\gamma_{\mathrm{pH}}=\left(1-10^{\mathrm{pH}_{\min }-\mathrm{pH}}\right)\end{equation}}
}
\examples{
modPresser(6,4.5,0.5)
modPresser(c(6,7),5,0.02)
}
