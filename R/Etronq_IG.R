#' Espérance tronquée d'une loi inverse gaussienne
#' @param d valeur où tronquer
#' @param mu mu
#' @param beta beta = dispersion * mu^2
#' @param dispersion dispersion = beta / mu^2
#' @export
Etronq_IG <- function(d, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    d -
        (2 * d - mu) *
        pnorm(q = (d - mu) *
                  sqrt(1 / (beta * d))) -
        (2 * d + mu) *
        exp(2 * mu / beta) *
        pnorm(q = - (d + mu) *
                  sqrt(1 / (beta * d)))
}
