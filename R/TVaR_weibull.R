#' Tail Value-at-risk d'une loi Weibull
#' @param kappa Niveau de confiance désiré
#' @param tau tau
#' @param beta beta
#' @export
TVaR_weibull <- function(kappa, tau, beta)
{
    1 / (beta * (1 - kappa)) *
        gamma(1 + 1/tau) *
        pgamma(q = -log(1 - kappa),
               shape = 1 + 1/tau,
               scale = 1,
               lower.tail = F)
}
