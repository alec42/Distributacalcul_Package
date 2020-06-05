#' Stop-loss of the Weibull distribution
#'
#' @description Stop-loss of the Weibull distribution with shape parameter
#'  parameter \eqn{\tau}{tau} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @template weibull-template
#'
#' @export
#' @importFrom stats pgamma
#'
#' @examples
#'
#' # With scale parameter
#' SL_weibull(d = 2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' SL_weibull(d = 2, shape = 3, rate = 0.25)
#'
SL_weibull <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0, d >= 0)

    E_weibull(shape, rate) *
        stats::pgamma(q = d^shape,
               shape = 1 + 1/shape,
               scale = rate^shape,
               lower.tail = FALSE) -
        d * exp(-(rate * d)^shape)
}
