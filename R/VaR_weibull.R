#' Value-at-risk d'une loi Weibull
#' @param kappa niveau de confiance désiré
#' @param tau tau
#' @param beta beta
#' @export
VaR_weibull <- function(kappa, tau, beta)
{
    1 / beta * (-log(1 - kappa))^(1/tau)
}
