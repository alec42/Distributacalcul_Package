#' Stop-loss d'une loi Gamma
#' @param d déductible
#' @param a alpha
#' @param b beta
#' @export
SL_gamma <- function(d, a, b)
{
    premier_morceau <- 1 - pgamma(d, a+1, b)
    deuxieme_morceau <- 1 - pgamma(d, a, b)
    (a/b)*premier_morceau - d*deuxieme_morceau
}
