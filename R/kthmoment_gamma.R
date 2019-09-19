#' k-ème moment de la loi gamma
#' @param k k-ème moment
#' @param shape alpha
#' @param rate (fréquence) beta, 1 par défaut.
#' @param scale (échelle) paramétrisation alternative où = 1 / rate.
#' @export
kthmoment_gamma <- function(k, shape, rate = 1, scale = 1 / rate)
{
    kthmoment_erlang(k, shape, rate)
}
dgamma()
