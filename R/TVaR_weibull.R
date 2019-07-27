#' Tail Value-at-risk d'une loi Weibull
#' @param k Niveau de confiance désiré
#' @param tau tau
#' @param beta beta
#' @export
TVaR_weibull <- function(k, tau, beta)
{
    1 / (beta * (1 - k)) *
        gamma(1 + 1/tau) *
        pgamma(q = -log(1 - k),
               shape = 1 + 1/tau,
               scale = 1,
               lower.tail = F)
}
