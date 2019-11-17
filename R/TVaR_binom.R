#' Tail Value-at-risk de la loi binomiale
#' @param kappa niveau de confiance désiré
#' @param n nombres d'essais de Bernouilli
#' @param p probabilité de succès (pour chaque expérience indépendantes)
#' @export
TVaR_binom <- function(kappa, n, p)
{
    k <- 0:n
    fx <- dbinom(k, n, p)
    ValueAtR <- qbinom(kappa, n, p)

    # Définition de la tvar, tout simplement
    un      <- sum((k * fx)[k > ValueAtR])
    deux    <- ValueAtR * (pbinom(ValueAtR, n, p) - kappa)
    sum (un, deux) / (1-kappa)
}
