#' Tail Value-at-risk d'une loi Log-logistique
#' @param kappa Niveau de confiance désiré
#' @param lam lam
#' @param tau tau
#' @export
TVaR_llogis <- function(kappa, lam, tau)
{
    lam / (1 - kappa) *
        gamma(1 + 1/tau) *
        gamma(1 - 1/tau) *
        pbeta(q = kappa,
              shape1 = 1 + 1/tau,
              shape2 = 1 - 1/tau,
              lower.tail = F)
}
