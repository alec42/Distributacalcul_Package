#' Stop-loss d'une loi Weibull
#' @param d montant d de la franchise
#' @param tau tau
#' @param beta beta
#' @export
SL_weibull <- function(d, tau, beta)
{
    1 / beta *
        gamma(1 + 1/tau) *
        pgamma(q = d^tau,
               shape = 1 + 1/tau,
               scale = beta^tau,
               lower.tail = F) -
        d * exp(-(beta * d)^tau)
}
