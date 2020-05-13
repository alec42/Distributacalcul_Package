#' kth moment of the Loglogistic distribution
#'
#' @description kth moment of the Loglogistic distribution with shape parameter
#'  \eqn{\tau}{tau} and scale parameter \eqn{\lambda}{lambda}.
#'
#' @templateVar k TRUE
#' @template loglogistic-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' kthmoment_llogis(k = 2, shape = 3, scale = 5)
#'
#' # With rate parameter
#' kthmoment_llogis(k = 2, shape = 3, rate = 0.2)
#'
kthmoment_llogis <- function(k, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, scale > 0, k > -shape, k < shape)

    scale^k * gamma(1 + k/shape) * gamma(1 - k/shape)
}
