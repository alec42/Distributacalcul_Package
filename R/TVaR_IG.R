#' Tail Value-at-Risk d'une loi inverse gaussienne
#'
#' @param kap Niveau de confiance désiré
#' @param mu mu
#' @param beta beta, = dispersion * mu^2
#' @param dispersion dispersion, = beta / mu^2
#' @param vark Value at Risk (VaR) calculé au même niveau de confiance kap
#' @details Cette formule nécessite la formule de la VaR_IG (déjà installée avec le package tvarPackage)
#' @export
TVaR_IG <- function(kap, mu, beta = dispersion * mu^2, dispersion = beta / mu^2, vark)
{
    (1/(1 - kap)) *
        (mu - vark +
             (2 * vark + mu) *
             exp(2 * mu / beta)
        ) +
        (1/(1 - kap)) *
        (
            (2 * vark - mu) *
                stats::pnorm(q = ((vark - mu) *
                               sqrt(1 / (beta * vark)))
                )
        )
}
