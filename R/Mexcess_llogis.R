#' Mean excess loss of the  Loglogistic Distribution
#'
#' @description Mean excess loss of the Loglogistic distribution with shape parameter
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
#' Mexcess_llogis(d = 2, shape = 3, scale = 5)
#'
#' # With rate parameter
#' Mexcess_llogis(d = 2, shape = 3, rate = 0.2)
#'
Mexcess_llogis <- function(d, shape, rate = 1/scale, scale = 1/rate)
{
    (d^shape + scale^shape) / (scale^(shape - 1)) *
        gamma(1 + 1/shape) *
        gamma(1 - 1/shape) *
        pbeta(q = (d^shape)/(scale^shape + d^shape),
              shape1 = 1 + 1/shape,
              shape2 = 1 - 1/shape,
              lower.tail = F) -
        d

}
