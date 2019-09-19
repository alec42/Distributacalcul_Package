#' k-ème moment d'une loi log-logistique
#' @param k k-ème moment
#' @param lam lambda
#' @param tau tau
#' @export
kthmoment_llogis <- function(k = 1, lam, tau)
{
    lam^k * gamma(1 + k/tau) * gamma(1 - k/tau)
}
