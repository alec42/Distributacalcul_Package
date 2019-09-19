#' Stop-loss d'une loi log-logistique
#' @param d montant d de la franchise
#' @param lam lambda
#' @param tau tau
#' @export
SL_llogis <- function(d, lam, tau)
{
    lam *
        gamma(1 + 1/tau) *
        gamma(1 - 1/tau) *
        pbeta(q = (d^tau)/(lam^tau + d^tau),
              shape1 = 1 + 1/tau,
              shape2 = 1 - 1/tau,
              lower.tail = F) -
        (d * (lam^tau)) /
        (lam^tau + d^tau)
}
