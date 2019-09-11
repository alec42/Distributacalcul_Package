#' Espérance d'une loi logarithmique
#' @param gam paramètre gamma
#' @export
E_logarithmique <- function(gam)
{
    (-gam) / (log(1 - gam) * (1 - gam))
}
