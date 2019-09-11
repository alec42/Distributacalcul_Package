#' Variance d'une loi logarithmique
#' @param gam paramètre gamma
#' @export
V_logarithmique <- function(gam)
{
    (gam + log(1 - gam)) / ((1 - gam)^2 * (log(1 - gam))^2)
}
