#' Truncated mean of the Loglogistic distribution
#'
#' @description Truncated mean of the Loglogistic distribution with
#'  shape parameter \eqn{\tau}{tau} and scale parameter
#'  \eqn{\lambda}{lambda}.
#'
#' @templateVar d TRUE
#' @templateVar less.than.d TRUE
#' @template loglogistic-template
#'
#' @export
#' @importFrom stats pbeta
#'
#' @examples
#'
#' # With scale parameter
#' Etrunc_llogis(d = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' Etrunc_llogis(d = 2, shape = 2, rate = 0.2)
#'
#' # values greather than d
#' Etrunc_llogis(d = 2, shape = 2, rate = 0.2, less.than.d = FALSE)
#'
Etrunc_llogis <- function(d, shape, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(d >= 0, shape > 1, scale > 0)

    if (less.than.d) {
        Etrunc.llogis <- E_llogis(shape, rate) * stats::pbeta(
            q = (d^shape)/(scale^shape + d^shape),
            shape1 = 1 + 1/shape,
            shape2 = 1 - 1/shape
        )
    } else {
        Etrunc.llogis <- E_llogis(shape, rate) * stats::pbeta(
            q = (d^shape)/(scale^shape + d^shape),
            shape1 = 1 + 1/shape,
            shape2 = 1 - 1/shape,
            lower.tail = FALSE
        )
    }

    return(Etrunc.llogis)
}
