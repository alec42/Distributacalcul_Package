#' Espérance d'une loi inverse gaussienne
#' @param mu mu
#' @param beta beta = dispersion * mu^2
#' @param dispersion dispersion = beta / mu^2
#' @export
E_IG <- function(mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    mu
}
