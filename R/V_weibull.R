#' Variance of the Weibull Distribution
#'
#' @description Variance of the Weibull distribution with shape parameter
#'  \eqn{\tau}{tau} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar kappa FALSE
#' @template weibull-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' V_weibull(shape = 2, scale = 5)
#'
#' # With rate parameter
#' V_weibull(shape = 2, rate = 0.2)
#'
V_weibull <- function(shape, rate = 1 / scale, scale = 1 / rate)
{
    kthmoment_weibull(shape, rate, k = 2) -
        kthmoment_weibull(shape, rate, k = 1)^2
}
