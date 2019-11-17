#' Variance d'une loi binomiale négative
#' Calcul la variance d'une binomiale négative
#' @param r nombre d'essais
#' @param q probabilité d'un succès indépendant
#' @export
V_nbinom <- function(r, q) r * ((1-q) / q^2)
