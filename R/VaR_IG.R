#' Value-at-risk of the Inverse Gaussian distribution
#'
#' @details Wrapper pour avoir une notation cohérente avec le reste des fonctions IG, mais utilise la fonction de densité du package ACTUAR.
#'
#' @param kap niveau de confiance désiré
#' @param mu mu
#' @param beta beta = dispersion * mu^2
#' @param dispersion dispersion = beta / mu^2
#'
#' @export
VaR_IG <- function(kap, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    require("actuar")
    qinvgauss(p = kap, mean = mu, dispersion = dispersion)
}
