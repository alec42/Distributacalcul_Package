#' Variance d'une loi Burr
#' @param alph alpha
#' @param lam lambda
#' @param rho rho
#' @export
V_burr <- function(lam, alph, rho)
{
    un <- gamma(1 + 2/rho) * gamma(alph - 2/rho)
    deux <- (gamma(1 + 1/rho)*gamma(alph - 1/rho))^2 / gamma(alph)
    trois <- lam^(2/rho) / gamma(alph)
    trois * (un - deux)
}
