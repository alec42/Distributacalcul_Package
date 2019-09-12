#' Envelope de la densité de la loi inverse gaussienne
#' @param x x
#' @param mu mu parameter
#' @param beta beta, = dispersion * mu^2
#' @param dispersion beta, = beta / mu^2
#' @details Wrapper pour avoir une notation cohérente avec le reste des fonctions IG, mais utilise la fonction de densité du package ACTUAR.
#' @export
dIG <- function(x, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    require("actuar")
    dinvgauss(x, mean = mu, dispersion = dispersion)
}
