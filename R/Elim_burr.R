#' Limited Mean of the Burr Distribution
#'
#' @description Limited expected value of the Burr distribution with shape
#'  parameter \eqn{\tau}{tau} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @templateVar kappa FALSE
#' @template burr-template
#'
#' @export
#'
#' @examples
#'
#' # With rate parameter
#' Elim_burr(d = 2, rate = 2, shape1 = 2, shape2 = 5)
#'
#' # With scale parameter
#' Elim_burr(d = 2, scale = 0.5, shape1 = 2, shape2 = 5)
#'
Elim_burr <- function(d, shape1, shape2, rate = 1 / scale, scale = 1 / rate)
{
    1 / gamma(shape1) *
        rate^(1/shape2) *
        gamma(1 + 1/shape2) *
        gamma(shape1 - 1/shape2) *
        pbeta(q = d^shape2 / (rate + d^shape2),
              shape1 = 1 + 1/shape2,
              shape2 = shape1 - 1/shape2) +
        d *
        (rate / (rate + d^shape2)) ^ shape1
}
