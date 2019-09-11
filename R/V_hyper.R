#' Variance d'une loi hypergéométrique
#' @param N nombre total
#' @param m nombre de type 1
#' @param n taille de l'échantillon
#' @export
V_hyper <- function(N, m, n)
{
    (n * (m / N)) * ((((n - 1) * (m - 1)) / (N - 1)) + 1 - (n * (m / N)))
}
