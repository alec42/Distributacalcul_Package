#' Espérance limitée d'une loi Burr
#' @param d déductible
#' @param alpha alpha
#' @param lam lambda
#' @param tau tau
#' @export
Elim_burr <- function(d, alpha, lam, tau) {
    1/gamma(alpha) *
        lam^(1/tau) *
        gamma(1 + 1/tau) *
        gamma(alpha - 1/tau) *
        pbeta(q = d^tau / (lam + d^tau),
              shape1 = 1 + 1/tau,
              shape2 = alpha - 1/tau) +
        d *
        (lam / (lam + d^tau)) ^ alpha
}
