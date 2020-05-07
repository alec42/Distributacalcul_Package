#' Mean excess loss of the Weibull distribution
#'
#' @description Mean excess loss of the Weibull distribution with shape parameter
#'  parameter \eqn{\tau}{tau} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @template weibull-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' Mexcess_weibull(d = 2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' Mexcess_weibull(d = 2, shape = 3, rate = 0.25)
#'
Mexcess_weibull <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    exp((rate * d)^shape) / rate *
        gamma(1 + 1/shape) *
        pgamma(q = d^shape,
               shape = 1 + 1/shape,
               scale = rate^shape,
               lower.tail = F) -
        d
}
