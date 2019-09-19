#' k-ème moment d'une loi Weibull
#' @param k k-ème moment
#' @param tau tau
#' @param beta beta
#' @export
kthmoment_weibull <- function(k = 1, tau, beta)
{
    1/(beta^k) * gamma(1 + k/tau)
}
