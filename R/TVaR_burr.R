#' Tail Value-at-risk d'une loi Burr
#' @param k Niveau de confiance désiré
#' @param var Value at Risk (VaR) calculé au même niveau de risque
#' @param alpha alpha
#' @param lam lam
#' @param tau tau
#' @details Cette formule nécessite la formule de la VaR_burr (déjà installée avec le package tvarPackage)
#' @export
TVaR_burr <- function(k, var, alpha, lam, tau) {
    1/((1 - k) * gamma(alpha)) *
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
