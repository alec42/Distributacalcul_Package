#' Tail Value-at-risk d'une loi gamma
#' @param kappa Niveau de confiance désiré
#' @param a alpha
#' @param b beta
#' @details Cette formule nécessite la formule VaR_gamma (déjà installée avec le package tvarPackage)
#' @export
TVaR_gamma <- function(kappa, a, b)
{
    survie <- 1 - pgamma(VaR_gamma(a, b, kappa),a + 1, b)
    (a * survie) / (b * (1-kappa))
}
