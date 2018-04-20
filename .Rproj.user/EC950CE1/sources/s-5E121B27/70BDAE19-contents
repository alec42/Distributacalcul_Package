#' Tail Value-at-risk de la loi Poisson
#' @param kappa niveau de confiance désiré
#' @param lam lambda, nombre d'occurences moyen dans un laps de temps donné.
#' @export
TVaR_pois <- function(kappa, lam)
{
    k <- 0:n
    fx <- dpois(k, lam)
    ValueAtR <- qpois(kappa, lam)

    # Définition de la tvar, tout simplement
    un      <- sum((k * fx)[k > ValueAtR])
    deux    <- ValueAtR * (ppois(ValueAtR, n, p) - kappa)
    sum (un, deux) / (1-kappa)
}
