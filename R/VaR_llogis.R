#' Value-at-risk de la loi Log-logstique
#' @param k niveau de confiance désiré
#' @param lam lambda
#' @param tau tau
#' @export
VaR_llogis <- function(k, lam, tau)
{
    lam * (k^(-1) - 1)^(-1/tau)
}
