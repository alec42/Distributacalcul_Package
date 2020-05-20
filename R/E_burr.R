#' Expected value of the Burr distribution
#'
#' @description Expected value of the Burr distribution with shape parameters
#'  \eqn{\alpha}{alpha} (shape1) and \eqn{\tau}{tau} (shape2) as well as
#'  rate parameter \eqn{\lambda}{lambda}.
#'
#' @template burr-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' E_burr(rate = 2, shape1 = 2, shape2 = 5)
#'
#' # With rate parameter
#' E_burr(scale = 0.5, shape1 = 2, shape2 = 5)
#'
E_burr <- function(shape1, shape2, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape1 > 0, shape2 > 0, rate > 0, shape1 * shape2 > 1)

    (rate^(1/shape2) / gamma(shape1)) *
        gamma(1 + 1/shape2) * gamma(shape1 - 1/shape2)
}
