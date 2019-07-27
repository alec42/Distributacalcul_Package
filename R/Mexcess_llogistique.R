#' Mean-Excess loss d'une loi Log-logistique
#'
#'
#' @param d déductible
#' @param lam lambda
#' @param tau tau
#' @export
Mexcess_llogis <- function(d, lam, tau)
{
    (d^tau + lam^tau) / (lam^(tau - 1)) *
        gamma(1 + 1/tau) *
        gamma(1 - 1/tau) *
        pbeta(q = (d^tau)/(lam^tau + d^tau),
              shape1 = 1 + 1/tau,
              shape2 = 1 - 1/tau,
              lower.tail = F) -
        d

}
