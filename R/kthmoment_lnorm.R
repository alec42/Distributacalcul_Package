#' k-ème moment de la loi Lognormale
#' @param k niveau de confiance désiré
#' @param mu mu
#' @param sig sigma = sqrt(sigma^2)
#' @export
kthmoment_lnorm <- function(k, mu, sig)
{
    exp(mu * k + k ^ 2 * (sig ^ 2) / 2)
}
