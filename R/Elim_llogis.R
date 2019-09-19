#' Espérance limitée d'une loi Log-logistique
#' @param d déductible
#' @param lam lambda
#' @param tau tau
#' @export
Elim_llogis <- function(d, lam, tau)
{
    lam *
        gamma(1 + 1/tau) *
        gamma(1 - 1/tau) *
        pbeta(q = (d^tau)/(lam^tau + d^tau),
              shape1 = 1 + 1/tau,
              shape2 = 1 - 1/tau) +
        (d * (lam^tau)) /
        (lam^tau + d^tau)
}
