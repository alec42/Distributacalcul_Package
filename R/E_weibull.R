#' Expected value of the Weibull distribution
#'
#' @description Expected value of the Weibull distribution with shape parameter
#'  \eqn{\tau}{tau} and rate parameter \eqn{\beta}{beta}.
#'
#' @template weibull-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' E_weibull(shape = 2, scale = 5)
#'
#' # With rate parameter
#' E_weibull(shape = 2, rate = 0.2)
#'
E_weibull <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0)

    1 / (rate) *
        gamma(1 + 1 / shape)
}
