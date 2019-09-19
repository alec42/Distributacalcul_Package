#' Mean-Excess loss (excès-moyen) d'une loi Burr
#' @param d déductible
#' @param alpha alpha
#' @param lam lambda
#' @param tau tau
#' @export
Mexcess_burr <- function(d, alpha, lam, tau) {
    (((lam + d ^ tau) ^ alpha) *
         gamma(1 + 1 / tau) *
         gamma(alpha - 1 / tau)) /
        ((lam ^ (alpha - 1 / tau)) *
             gamma(alpha)) *
        pbeta(
            q = (d ^ tau) / (lam + (d ^ tau)),
            shape1 = 1 + 1 / tau,
            shape2 = alpha - 1 / tau,
            lower.tail = F) - d
}
