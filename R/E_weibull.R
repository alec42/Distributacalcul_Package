#' k-ème moment d'une loi Weibull
#' @param tau tau
#' @param beta beta
#' @export
E_weibull <- function(tau, beta)
{
    1/(beta) * gamma(1 + 1/tau)
}
