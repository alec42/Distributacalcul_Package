#' Value-at-Risk of the Loglogistic distribution
#'
#' @description Value-at-Risk of the Loglogistic distribution with shape
#'  parameter \eqn{\tau}{tau} and scale parameter \eqn{\lambda}{lambda}.
#'
#' @templateVar kap TRUE
#' @template loglogistic-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' VaR_llogis(kap = 0.8, shape = 3, scale = 5)
#'
#' # With rate parameter
#' VaR_llogis(kap = 0.8, shape = 3, rate = 0.2)
#'
VaR_llogis <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(kap >= 0, kap <= 1, shape > 0, scale > 0)

    scale * (kap^(-1) - 1)^(-1/shape)
}
