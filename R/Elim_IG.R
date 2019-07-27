#' Espérance limitée d'une loi Inverse Gaussiennee
#' @param d montant d de la limite
#' @param mu mu
#' @param beta beta, = dispersion * mu^2
#' @param dispersion beta, = beta / mu^2
#' @export
Elim_IG <- function(d, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    levinvgauss(limit = d, mean = mu, dispersion = dispersion, order = 1)
}
