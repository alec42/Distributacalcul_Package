#' Envelope de la densité de la loi inverse gaussienne
#' @param q quantile
#' @param mu mu
#' @param beta beta = dispersion * mu^2
#' @param dispersion dispersion = beta / mu^2
#' @param lower.tail Si vrai (défaut) alors fonction de répartition. Si faux, de survie.
#' @details Wrapper pour avoir une notation cohérente avec le reste des fonctions IG, mais utilise la fonction de densité du package ACTUAR.
#' @export
#' @importFrom actuar pinvgauss
pIG <- function(q, mu, beta = dispersion * mu^2, dispersion = beta / mu^2, lower.tail = T)
{
    actuar::pinvgauss(q = q, mean = mu, dispersion = dispersion, lower.tail = lower.tail)
}
