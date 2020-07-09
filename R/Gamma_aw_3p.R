#' this function of secondary growth model describe the evolution of the square root of the maximum specific growth rate (sqrtmumax) as a function of water activity (aw), This is a symetric cardinal pH model developped by Rosso & Robinson in 2001 with three parameters (awmin, awopt, muopt)
#'@details
#'\deqn{\begin{equation}\begin{aligned}
#'&\begin{array}{l}
#'\mathrm{cpm}_{-} \mathrm{aw}_{-} 3 \mathrm{p}<-\text { as.formula }\left(\mathrm{sqrtmumax}^{\sim} \underline{\mathrm{sqrt}}(\mathrm{aw}>=\mathrm{awmin})^{*} \text { muopt }^{*}(\mathrm{aw}-1)^{*}\left(\mathrm{aw}^{-}\right.\right. \\
#'\text {awmin } )^{\wedge} 2 /\left((\text { awopt }-\mathrm{awmin})^{*}\left((\text { awopt }-\mathrm{awmin})^{*}(\mathrm{aw}-\mathrm{awopt})-(\mathrm{awopt}-1)^{*}(\text { awopt }+\right.\right.
#'\end{array}\\
#'&\text { awmin }-2.0 * \mathrm{aw} \text { ) })))
#'\end{aligned}\end{equation}}
#' @param aw water activity #a number
#' @param awmin minimal growth water activity # a number
#' @param awopt optimal growth water activity # a number
#' @param muopt Optimal growth rate # a number # a number
#'
#' @return sqrmumax^2= mumax #maximum growth rate # a number
#' @export
#'
#' @examples
#' Gamma_aw_3p(0.95,0.90, 0.98, 0.5)
Gamma_aw_3p<-function(aw,awmin,awopt,muopt)
{mumax <- ((aw >= awmin) * muopt * (aw - 1)*(aw - awmin)^2 / ((awopt - awmin) * ((awopt - awmin) * (aw - awopt) - (awopt - 1) * (awopt + awmin - 2 * aw))))
return((mumax))
}

