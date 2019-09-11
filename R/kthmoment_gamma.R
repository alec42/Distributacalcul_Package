#' k-ème moment de la loi gamma
#' @param k k-ème moment
#' @param shape alpha
#' @param rate beta
#' @export
kthmoment_gamma <- function(k, shape, rate)
{
    kthmoment_erlang(k, shape, rate)
}
