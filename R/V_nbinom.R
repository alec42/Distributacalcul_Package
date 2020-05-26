#' Variance d'une loi binomiale négative
#' Calcul la variance d'une binomiale négative
#' @param r nombre d'essais
#' @param q probabilité d'un succès indépendant
#' @export
V_nbinom <- function(r, q) {
    .Deprecated("V_negbinom")
    r * ((1 - q) / q^2)
}
