#' Espérance limitée d'une loi gamma
#' @param d déductible
#' @param a alpha
#' @param b beta
#' @export
Elim_gamma <- function(d, a, b)
{
    un   <- (a/b) *  pgamma(d, a+1, b)
    deux <- d * (1 - pgamma(d,  a, b))
    un  + deux
}
