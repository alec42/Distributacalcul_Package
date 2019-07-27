#' Espérance tronquée d'une loi Log-logistique
#' @param d Deductible
#' @param lam lambda
#' @param tau tau
#' @export
Etronq_llogis <- function(d, lam, tau)
{
    lam *
        gamma(1 + 1/tau) *
        gamma(1 - 1/tau) *
        pbeta(q = (d^tau)/(lam^tau + d^tau),
              shape1 = 1 + 1/tau,
              shape2 = 1 - 1/tau)
}
