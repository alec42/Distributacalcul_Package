#' Espérance tronquée d'une loi Weibull
#' @param d valeur où tronquer
#' @param tau tau
#' @param beta beta
#' @export
Etronq_weibull <- function(d, tau, beta)
{
    1 / beta *
        gamma(1 + 1/tau) *
        pgamma(q = d^tau,
               shape = 1 + 1/tau,
               scale = beta^tau)
}
