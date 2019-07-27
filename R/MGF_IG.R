#' Fonction génératrice des moments de la loi Inverse Gaussienne
#' @param t t
#' @param mu mu
#' @param beta beta = dispersion * mu^2
#' @param dispersion dispersion = beta / mu^2
#' @export
MGF_IG <- function(t, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    exp((mu/beta) * (1 - sqrt(1 - 2 * beta * t)))
}
