#' Mean-Excess loss of the Burr Distribution
#'
#' @description Mean excess loss of the Burr distribution with shape parameters
#'  \eqn{\alpha}{alpha} (shape1) and \eqn{\tau}{tau} (shape2) as well as rate parameter
#'  \eqn{\lambda}{lambda}.
#'
#' @templateVar d TRUE
#' @templateVar kappa FALSE
#' @template burr-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' Mexcess_burr(d = 2, rate = 2, shape1 = 2, shape2 = 5)
#'
#' # With rate parameter
#' Mexcess_burr(d = 2, scale = 0.5, shape1 = 2, shape2 = 5)
#'
Mexcess_burr <- function(d, shape1, shape2, rate = 1 / scale, scale = 1 / rate) {
    (
        ((rate + d^shape2)^shape1) *
            gamma(1 + 1/shape2) *
            gamma(shape1 - 1/shape2)
    ) / (
        (rate ^ (shape1 - 1/shape2)) *
            gamma(shape1)
    ) *
        pbeta(
            q = (d^shape2) / (rate + (d^shape2)),
            shape1 = 1 + 1/shape2,
            shape2 = shape1 - 1/shape2,
            lower.tail = F) -
        d
}
