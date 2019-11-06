#' kth Moment of the Erlang Distribution
#'
#' @description kth-moment of the Weibull distribution with shape parameter
#'  parameter \eqn{\tau}{tau} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar k TRUE
#' @templateVar kappa FALSE
#' @template weibull-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' kthmoment_weibull(k = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' kthmoment_weibull(k = 2, shape = 2, rate = 0.2)
#'
kthmoment_weibull <- function(k = 1, shape, rate = 1 / scale, scale = 1 / rate)
{
    1 / (rate^k) *
        gamma(1 + k / shape)
}
