#' Expected value of the Loglogistic distribution
#'
#' @description Expected value of the Loglogistic distribution with shape
#'  parameter \eqn{\tau}{tau} and scale parameter \eqn{\lambda}{lambda}.
#'
#' @template loglogistic-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' E_llogis(shape = 3, scale = 5)
#'
#' # With rate parameter
#' E_llogis(shape = 3, rate = 0.2)
#'
E_llogis <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 1, scale > 0)

    scale * gamma(1 + 1/shape) * gamma(1 - 1/shape)
}
