#' Variance d'une loi log-logstique
#' @param lam lambda
#' @param tau tau
#' @export
V_llogis <- function(lam, tau)
{
    (kthmoment_llogis(k = 2, lam, tau) - (kthmoment_llogis(k = 1, lam, tau)^2))
}
