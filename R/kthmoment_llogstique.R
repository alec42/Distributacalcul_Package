#' k-ème moment de la loi Log-logistique
#' @param k niveau de confiance désiré
#' @param lam lambda
#' @param tau tau
#' @export
kthmoment_llogis <- function(k = 1, lam, tau)
{
    lam^k * gamma(1 + k/tau) * gamma(1 - k/tau)
}
