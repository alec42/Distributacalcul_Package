#' Value-at-risk d'une loi gamma
#' @param kappa Niveau de confiance désiré
#' @param a alpha
#' @param b beta
#' @details Pour trouver la VaR d'une gamme, on peut aussi procéder par optimisation numérique (`uniroot()` ou `optimize()`)
#' @export
VaR_gamma <- function(kappa, a, b)
{
    qgamma(kappa, a, b)
}
