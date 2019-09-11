#' k-ème moment de la loi Weibull
#' @param k k-ème moment
#' @param tau tau
#' @param beta beta
#' @export
kthmoment_weibull <- function(tau, beta, k = 1)
{
    1/(beta^k) * gamma(1 + k/tau)
}
