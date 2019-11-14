#' Tail Value-at-Risk of the Loglogistic Distribution
#'
#' @description Tail Value-at-Risk of the Loglogistic distribution with shape parameter
#'  \eqn{\tau}{tau} and scale parameter \eqn{\lambda}{lambda}.
#'
#' @templateVar q FALSE
#' @templateVar kappa TRUE
#' @template loglogistic-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' TVaR_llogis(kappa = 0.8, shape = 3, scale = 5)
#'
#' # With rate parameter
#' TVaR_llogis(kappa = 0.8, shape = 3, rate = 0.2)
#'
TVaR_llogis <- function(kappa, shape, rate = 1/scale, scale = 1/rate)
{
    scale / (1 - kappa) *
        gamma(1 + 1/shape) *
        gamma(1 - 1/shape) *
        pbeta(q = kappa,
              shape1 = 1 + 1/shape,
              shape2 = 1 - 1/shape,
              lower.tail = F)
}
