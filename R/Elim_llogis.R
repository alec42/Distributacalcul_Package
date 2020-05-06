#' Limited Mean of the Loglogistic distribution
#'
#' @description Limited expected value of the Loglogistic distribution with shape parameter
#'  \eqn{\tau}{tau} and scale parameter \eqn{\lambda}{lambda}.
#'
#' @templateVar d TRUE
#' @templateVar q FALSE
#' @templateVar kappa FALSE
#' @template loglogistic-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' Elim_llogis(d = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' Elim_llogis(d = 2, shape = 2, rate = 0.2)
#'
Elim_llogis <- function(d, shape, rate = 1/scale, scale = 1/rate)
{
    scale *
        gamma(1 + 1/shape) *
        gamma(1 - 1/shape) *
        pbeta(q = (d^shape)/(scale^shape + d^shape),
              shape1 = 1 + 1/shape,
              shape2 = 1 - 1/shape) +
        (d * (scale^shape)) /
        (scale^shape + d^shape)
}
