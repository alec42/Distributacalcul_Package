#' Tail Value-at-Risk of the Loglogistic distribution
#'
#' @description Tail Value-at-Risk of the Loglogistic distribution with shape parameter
#'  \eqn{\tau}{tau} and scale parameter \eqn{\lambda}{lambda}.
#'
#' @templateVar q FALSE
#' @templateVar kap TRUE
#' @template loglogistic-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' TVaR_llogis(kap = 0.8, shape = 3, scale = 5)
#'
#' # With rate parameter
#' TVaR_llogis(kap = 0.8, shape = 3, rate = 0.2)
#'
TVaR_llogis <- function(kap, shape, rate = 1/scale, scale = 1/rate)
{
    scale / (1 - kap) *
        gamma(1 + 1/shape) *
        gamma(1 - 1/shape) *
        pbeta(q = kap,
              shape1 = 1 + 1/shape,
              shape2 = 1 - 1/shape,
              lower.tail = F)
}
