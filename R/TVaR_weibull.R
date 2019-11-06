#' Tail Value-at-Risk of the Weibull Distribution
#'
#' @description Tail Value-at-Risk of the Weibull distribution with shape parameter
#'  parameter \eqn{\tau}{tau} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar kappa TRUE
#' @template weibull-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' TVaR_weibull(kappa = .2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' TVaR_weibull(kappa = .2, shape = 3, rate = 0.25)
#'
TVaR_weibull <- function(kappa, shape, rate = 1 / scale, scale = 1 / rate)
{
    1 / (rate * (1 - kappa)) *
        gamma(1 + 1/shape) *
        pgamma(q = -log(1 - kappa),
               shape = 1 + 1/shape,
               scale = 1,
               lower.tail = F)
}
