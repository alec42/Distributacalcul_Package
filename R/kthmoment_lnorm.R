#' k-ème moment d'une loi lognormale
#' @param k k-ème moment
#' @param mu mu
#' @param sig sigma = sqrt(sigma^2)
#' @export
kthmoment_lnorm <- function(k, mu, sig)
{
    exp(mu * k + k ^ 2 * (sig ^ 2) / 2)
}
