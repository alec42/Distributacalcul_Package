#' kth moment of the Weibull distribution
#'
#' @description kth moment of the Weibull distribution with shape parameter
#'  parameter \eqn{\tau}{tau} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar k TRUE
#' @template weibull-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' kthmoment_weibull(k = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' kthmoment_weibull(k = 2, shape = 2, rate = 0.2)
#'
kthmoment_weibull <- function(k, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0) # restricton on k?

    1 / (rate^k) *
        gamma(1 + k / shape)
}
