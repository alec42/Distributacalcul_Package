#' Value-at-risk d'une loi gamma
#' @param kappa Niveau de confiance désiré
#' @param a alpha
#' @param b beta
#' @export
VaR_gamma <- function(kappa, a, b)
{
    # On procède par optimisation
    f <- function(x) pgamma(x, a, b) - kappa
    uniroot(f, c(0,99999999999))$root
    # Note : la fonction qgamma nous donne la réponse aussi,
    # mais aucun gain au niveau de la vitesse... je garde l'algo
    # avec Uniroot
}
