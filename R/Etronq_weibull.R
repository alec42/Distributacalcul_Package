#' Truncated Mean of the Weibull Distribution
#'
#' @description Truncated mean of the Weibull distribution with shape parameter
#'  parameter \eqn{\tau}{tau} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @templateVar kappa FALSE
#' @template weibull-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' Etronq_weibull(d = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' Etronq_weibull(d = 2, shape = 2, rate = 0.2)
#'
Etronq_weibull <- function(d, shape, rate = 1 / scale, scale = 1 / rate)
{
    1 / rate *
        gamma(1 + 1/shape) *
        pgamma(q = d^shape,
               shape = 1 + 1/shape,
               scale = rate^shape)
}
