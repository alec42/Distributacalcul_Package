#' Tail Value-at-risk d'une loi Log-logistique
#' @param k Niveau de confiance désiré
#' @param lam lam
#' @param tau tau
#' @export
TVaR_llogis <- function(k, lam, tau)
{
    lam / (1 - k) *
        gamma(1 + 1/tau) *
        gamma(1 - 1/tau) *
        pbeta(q = k,
              shape1 = 1 + 1/tau,
              shape2 = 1 - 1/tau,
              lower.tail = F)
}
