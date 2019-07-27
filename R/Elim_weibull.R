#' Espérance limitée d'une loi Weibull
#' @param d déductible
#' @param tau tau
#' @param beta beta
#' @export
Elim_weibull <- function(d, tau, beta)
{
    1 / beta *
        gamma(1 + 1/tau) *
        pgamma(q = d^tau,
               shape = 1 + 1/tau,
               scale = beta^tau) +
        d * exp(-(beta * d)^tau)
}
