#' k-ème moment de la loi Burr
#' @param k niveau de confiance désiré, -tau < k < alpha * tau
#' @param alpha alpha
#' @param lam lambda
#' @param tau tau
#' @export
kthmoment_burr <- function(k, alpha, lam, tau)
{
    (1/gamma(alpha)) *
        lam^(k/tau) *
        gamma(1 + k/tau) *
        gamma(alpha - k/tau)
}
