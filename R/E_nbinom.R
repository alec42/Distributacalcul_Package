#' Espérance d'une loi binomiale négative
#' Calcul l'espérance d'une binomiale négative
#' @param r nombre d'essais
#' @param q probabilité d'un succès indépendant
#' @export
E_nbinom <- function(r, q) r * ((1-q) / q )
