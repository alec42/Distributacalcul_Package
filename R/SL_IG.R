#' Stop-loss d'une loi inverse gaussienne
#'
#' @param d montant d de la franchise
#' @param mu mu
#' @param beta beta = dispersion * mu^2
#' @param dispersion dispersion = beta / mu^2
#' @export
SL_IG <- function(d, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    (mu - d) *
        pnorm(q = (d - mu) *
                  sqrt(1 / (beta * d)),
              lower.tail = F) +
        (d + mu) *
        exp(2 * mu / beta) *
        pnorm(q = - (d + mu) *
                  sqrt(1 / (beta * d)))
}
