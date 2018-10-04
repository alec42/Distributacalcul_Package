#' Tail Value-at-risk de Tx pour la loi Gompertz
#' @param kappa niveau de confiance pour l'évaluation de la TVaR
#' @param x l'âge atteint de l'assuré.
#' @param beta paramètre beta
#' @param gam paramètre gamma
#' @details Cette fonction nécessite d'avoir loadé le package `tvarPackage` au complet, car elle fait appel à une autre fonction (celle équivalente à la VaR) `qgompertz`.
#' @export
TVaRtx_gompertz <- function(kappa, x = 0, beta, gam)
{
    # Approche très théorique, pas de forme explicite.
    f <- function(t) qgompertz(t, x, beta, gam, Tx = T)
    integrate(f, kappa, 1)$ value / (1-kappa)
}
