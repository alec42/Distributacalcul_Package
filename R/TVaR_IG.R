#' Tail Value-at-risk d'une loi Inverse Gaussienne
#' @param k Niveau de confiance désiré
#' @param vark Value at Risk (VaR) calculé au même niveau de confiance k
#' @param mu mu
#' @param beta beta, = dispersion * mu^2
#' @param dispersion dispersion, = beta / mu^2
#' @details Cette formule nécessite la formule de la VaR_IG (déjà installée avec le package tvarPackage)
#' @export
TVaR_IG <- function(k, vark, mu, beta = dispersion * mu^2, dispersion = beta / mu^2)
{
    (1/(1 - k)) *
        (mu - vark +
             (2 * vark + mu) *
             exp(2 * mu / beta)
        ) +
        (1/(1 - k)) *
        (
            (2 * vark - mu) *
                pnorm(q = ((vark - mu) *
                               sqrt(1 / (beta * vark)))
                )
        )
}
