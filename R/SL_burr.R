#' Stop-loss d'une loi Burr
#' @param d montant d de la franchise
#' @param alpha alpha
#' @param lam lambda
#' @param tau tau
#' @export
SL_burr <- function(d, alpha, lam, tau) {
    1/(gamma(alpha)) *
        (lam^(1/tau)) *
        gamma(1 + 1/tau) *
        gamma(alpha - 1/tau) *
        pbeta(q = (d^tau / (lam + (d^tau))),
              shape1 = 1 + 1/tau,
              shape2 = alpha - 1/tau,
              lower.tail = F) -
        d *
        (lam / (lam + d^tau)) ^ alpha
}
