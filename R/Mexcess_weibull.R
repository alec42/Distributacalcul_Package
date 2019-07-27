#' Mean-Excess loss d'une loi Weibull
#'
#'
#' @param d déductible
#' @param tau tau
#' @param beta beta
#' @export
Mexcess_weibull <- function(d, tau, beta)
{
    exp((beta * d)^tau) / beta *
        gamma(1 + 1/tau) *
        pgamma(q = d^tau,
               shape = 1 + 1/tau,
               scale = beta^tau,
               lower.tail = F) -
        d
}
