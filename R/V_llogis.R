#' Variance of the Loglogistic Distribution
#'
#' @description Variance of the Loglogistic distribution with shape parameter
#'  \eqn{\tau}{tau} and scale parameter \eqn{\lambda}{lambda}.
#'
#' @templateVar q FALSE
#' @templateVar kappa FALSE
#' @template loglogistic-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' V_llogis(shape = 3, scale = 5)
#'
#' # With rate parameter
#' V_llogis(shape = 3, rate = 0.2)
#'
V_llogis <- function(shape, rate = 1/scale, scale = 1/rate)
{
    (kthmoment_llogis(k = 2, shape, rate) - (kthmoment_llogis(k = 1, shape, rate)^2))
}
