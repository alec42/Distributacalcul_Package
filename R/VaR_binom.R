#' Value-at-risk de la loi binomiale
#' @param kappa niveau de confiance désiré
#' @param n nombres d'essais de Bernouilli
#' @param p probabilité de succès (pour chaque expérience indépendantes)
#' @export
VaR_binom <- function(kappa, n, p) qbinom(kappa, n, p)
