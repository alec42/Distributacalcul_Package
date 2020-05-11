#' Limited mean of the Weibull distribution
#'
#' @description Limited mean of the Weibull distribution with shape
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
#' Elim_weibull(d = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' Elim_weibull(d = 2, shape = 2, rate = 0.2)
#'
Elim_weibull <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0, d >= 0)

    E_weibull(shape, rate) *
        stats::pgamma(q = d^shape,
               shape = 1 + 1/shape,
               scale = rate^shape) +
        d * exp(-(rate * d)^shape)
}
