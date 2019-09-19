#' Value-at-risk d'une loi inverse gaussienne
#' @param kappa niveau de confiance désiré
#' @param mu mu
#' @param beta beta = dispersion * mu^2
#' @param dispersion dispersion = beta / mu^2
#' @details Wrapper pour avoir une notation cohérente avec le reste des fonctions IG, mais utilise la fonction de densité du package ACTUAR.
#' @export
VaR_IG <- function(kappa, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    require("actuar")
    qinvgauss(p = kappa, mean = mu, dispersion = dispersion)
}
