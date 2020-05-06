#' Value-at-Risk of the Loglogistic distribution
#'
#' @description Value-at-Risk of the Loglogistic distribution with shape parameter
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
#' VaR_llogis(kappa = 0.8, shape = 3, scale = 5)
#'
#' # With rate parameter
#' VaR_llogis(kappa = 0.8, shape = 3, rate = 0.2)
#'
VaR_llogis <- function(kappa, shape, rate = 1/scale, scale = 1/rate) {
    scale * (kappa^(-1) - 1)^(-1/shape)
}
