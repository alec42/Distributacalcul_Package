#' Espérance d'une loi Burr
#' @param alph alpha
#' @param lam lambda
#' @param rho rho
#' @export
E_burr <- function(lam, alph, rho)
{
    un <- lam^(1/rho) * gamma(1 + 1/rho) * gamma(alph - 1/rho)
    un / gamma(alph)
}
