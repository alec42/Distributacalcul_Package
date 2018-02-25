#' Variance d'une loi binomiale
#' Calcul la variance d'une loi binomiale
#' @param n nombre d'essais
#' @param q probabilité d'un succès indépendant
#' @export
V_binom <- function(n, q) n * q * (1 - q)
