#' Tail Value-at-risk d'une loi Burr
#' @param kappa Niveau de confiance désiré
#' @param alpha alpha
#' @param lam lam
#' @param tau tau
#' @param vark Value at Risk (VaR) calculé au même niveau de risque
#' @details Cette formule nécessite la formule de la VaR_burr (déjà installée avec le package tvarPackage)
#' @export
TVaR_burr <- function(kappa, alpha, lam, tau, vark) {
    1/((1 - kappa) * gamma(alpha)) *
        (
            (lam^(1 / tau)) *
                gamma(1 + 1 / tau) *
                gamma(alpha - 1 / tau) *
                pbeta(q = (var^tau) / (lam + var^tau),
                      shape1 = 1 + 1 / tau,
                      shape2 = alpha - 1 / tau,
                      lower.tail = F)
        )

}
