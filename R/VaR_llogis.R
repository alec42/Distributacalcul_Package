#' Value-at-risk d'une loi log-logstique
#' @param kappa niveau de confiance désiré
#' @param lam lambda
#' @param tau tau
#' @export
VaR_llogis <- function(kappa, lam, tau)
{
    lam * (kappa^(-1) - 1)^(-1/tau)
}
