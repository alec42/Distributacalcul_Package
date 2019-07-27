#' Value-at-risk de la loi Inverse Gaussienne
#' @param p niveau de confiance désiré
#' @param mu mu
#' @param beta beta, = dispersion * mu^2
#' @param dispersion dispersion, = beta / mu^2
#' @export
VaR_IG <- function(k, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    qinvgauss(p = k, mean = mu, dispersion = dispersion)
}
