#' Truncated Mean of the Loglogistic Distribution
#'
#' @description Truncated mean of the Loglogistic distribution with shape parameter
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
#' Etronq_llogis(d = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' Etronq_llogis(d = 2, shape = 2, rate = 0.2)
#'
Etronq_llogis <- function(d, shape, rate = 1/scale, scale = 1/rate)
{
    scale *
        gamma(1 + 1/shape) *
        gamma(1 - 1/shape) *
        pbeta(q = (d^shape)/(scale^shape + d^shape),
              shape1 = 1 + 1/shape,
              shape2 = 1 - 1/shape)
}
