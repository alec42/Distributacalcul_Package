#' Value-at-risk de la loi Weibull
#' @param k niveau de confiance désiré
#' @param tau tau
#' @param beta beta
#' @export
VaR_weibull <- function(k, tau, beta)
{
    1 / beta * (-log(1 - k))^(1/tau)
}
