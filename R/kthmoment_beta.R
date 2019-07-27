#' k-ème moment de la loi Beta
#' @param k niveau de confiance désiré
#' @param a alpha
#' @param b beta
#' @export
kthmoment_beta <- function(k, a, b)
{
    (gamma(a + k) * gamma(a + b))/(gamma(a) * gamma(a + b + k))
}
