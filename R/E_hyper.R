#' Espérance d'une loi hypergéométrique
#' @param N nombre total
#' @param m nombre de type 1
#' @param n taille de l'échantillon
#' @export
E_hyper <- function(N, m, n)
{
    n * (m / N)
}
