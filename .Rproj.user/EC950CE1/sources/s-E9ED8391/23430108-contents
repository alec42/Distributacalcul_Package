#' Mean-Excess loss d'une loi gamma
#'
#'
#' @param d déductible
#' @param a alpha
#' @param b beta
#' @export
Mexcess_gamma <- function(d, a, b)
{
    numerateur   <- a * (1 - pgamma(d, a+1, b))
    denominateur <- b * (1 - pgamma(d, a  , b))
    (numerateur/denominateur) - d
}
