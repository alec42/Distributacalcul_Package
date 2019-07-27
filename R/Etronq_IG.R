#' Espérance tronquée d'une loi Inverse Gaussienne
#' @param d Deductible
#' @param alpha alpha
#' @param lam lambda
#' @param tau tau
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
