#' Variance d'une loi Weibull
#' @param tau tau
#' @param beta beta
#' @export
V_weibull <- function(tau, beta)
{
    (kthmoment_weibull(tau = tau, beta = beta, k = 2) - (kthmoment_weibull(tau = tau, beta = beta, k = 1))^2)
}
