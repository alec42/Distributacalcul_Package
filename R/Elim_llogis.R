#' Limited mean of the Loglogistic distribution
#'
#' @description Limited mean of the Loglogistic distribution with
#'  shape parameter \eqn{\tau}{tau} and scale parameter
#'  \eqn{\lambda}{lambda}.
#'
#' @templateVar d TRUE
#' @template loglogistic-template
#'
#' @export
#' @importFrom stats pbeta
#'
#' @examples
#'
#' # With scale parameter
#' Elim_llogis(d = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' Elim_llogis(d = 2, shape = 2, rate = 0.2)
#'
Elim_llogis <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(d >= 0, shape > 1, scale > 0)

    E_llogis(shape, rate) *
        stats::pbeta(
            q = (d^shape)/(scale^shape + d^shape),
            shape1 = 1 + 1/shape,
            shape2 = 1 - 1/shape
        ) +
        d * ((scale^shape) / (scale^shape + d^shape))
}
