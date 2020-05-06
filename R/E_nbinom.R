#' Espérance d'une loi binomiale négative
#' Calcul l'espérance d'une binomiale négative
#' @param r nombre d'essais
#' @param q probabilité d'un succès indépendant
#' @export
E_nbinom <- function(r, q) {
    .Deprecated("E_negbinom")
    r * ((1-q) / q )
}
