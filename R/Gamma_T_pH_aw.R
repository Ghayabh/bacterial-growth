#' This model describe the evolution of the square root of the maximum specific growth rate (sqrtmumax) as a function of temperanture, pH & water activty(aw), It is a cardinal model based on the gamma concept, developed by Pinon et al. in 2004 with 9 parameters (Tmin, Topt, Tmax, pHmin, pHopt, pHmax, awmin, awopt, muopt)
#'@details
#'\deqn{\begin{equation}\begin{array}{l}
#'\mathrm{cpm}_{-} \mathrm{T}_{-} \mathrm{pH}_{-} \mathrm{aw}<-\text { as.formula(sqrtmumax }^{\sim} \operatorname{sqrt}(((\mathrm{T}>= \\
#' \mathrm{Tmin}) \&(\mathrm{T}<=\mathrm{Tmax}) \&(\mathrm{pH}>=\mathrm{pHmin}) \&(\mathrm{pH}<= \\
#' (\mathrm{pHmax})) \&(\mathrm{aw}>=\mathrm{awmin}))^{*} \text { muopt }^{*}(\mathrm{T}-\mathrm{Tmax})^{*}(\mathrm{T}- \\
#' \mathrm{Tmin})^{\wedge} 2 /\left((\mathrm{Topt}-\mathrm{Tmin})^{*}\left((\mathrm{Topt}-\mathrm{Tmin})^{*}(\mathrm{T}-\mathrm{Topt})-\right.\right. \\
#' \left.\left.(\mathrm{Topt}-\mathrm{Tmax})^{*}\left(\mathrm{Topt}+\mathrm{Tmin}-2.0^{*} \mathrm{T}\right)\right)\right)^{*}(\mathrm{pH}-\mathrm{pHmin}) \\
#' *(\mathrm{pH}-\mathrm{pHmax}) /\left((\mathrm{pH}-\mathrm{pHmin})^{*}(\mathrm{pH}-\mathrm{pHmax})-(\mathrm{pH}-\right. \\
#' \left.\mathrm{pHopt})^{\wedge} 2\right)^{*}(\mathrm{aw}-1)^{*}(\mathrm{aw}-\mathrm{awmin})^{\wedge} 2 /( \text { (awopt - } \\
#' \mathrm{awmin})^{*}\left((\text { awopt - awmin })^{*}(\mathrm{aw}-\text { awopt })-(\text { awopt - } 1)\right) \\
#' \left.\left.\left.\left.*\left(\mathrm{awopt}+\mathrm{awmin}-2.0^{*} \mathrm{aw}\right)\right)\right)\right)\right)
#'\end{array}\end{equation}}
#' @param T Temperature # a number
#' @param Tmin Minimal growth temperature #a number
#' @param Tmax Maximal growth temperature # a number
#' @param Topt Optimal growth temperature #a number
#' @param pH pH #a number
#' @param pHmin Minimal growth pH #a number
#' @param pHmax Maximal growth pH #a number
#' @param pHopt Optimal growth pH #a number
#' @param aw Water activity #number
#' @param awmin Minimal growth water activity # a number
#' @param awmax Maximal growth water activity # a number
#' @param awopt Optimal growth water activity # a number
#' @param muopt Optimal growth rate # a number # a number
#'
#' @return sqrmumax^2= mumax #maximum growth rate # a number
#' @export
#'
#' @examples
#' Gamma_T_pH_aw(10,4,45,37,5,4,9,7,0.92,0.90,0.98,0.97,0.5)
Gamma_T_pH_aw <- function(T,Tmin,Tmax,Topt,pH,pHmin,pHmax,pHopt,aw,awmin,awmax,awopt,muopt)
{sqrtmumax<-sqrt(((T >= Tmin) & (T <= Tmax) & (pH >= pHmin) & (pH <= (pHmax)) & (aw >= awmin))
                 * muopt * (T-Tmax) * (T - Tmin)^2 / ((Topt - Tmin) *
                                                        ((Topt - Tmin) * (T - Topt) - (Topt - Tmax) * (Topt + Tmin - 2.0 * T)))
                 * (pH - pHmin) * (pH - pHmax) / ((pH - pHmin) * (pH - pHmax) - (pH - pHopt)^2)
                 * (aw - 1) * (aw - awmin)^2 / ((awopt - awmin) * ((awopt - awmin) * (aw - awopt) - (awopt - 1)
                                                                   * (awopt + awmin - 2.0 * aw))))
return((sqrtmumax ^ 2))
}
