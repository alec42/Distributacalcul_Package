#' Value-at-Risk of the Burr Distribution
#'
#' @description Value-at-Risk of the Burr distribution with shape parameters
#'  \eqn{\alpha}{alpha} (shape1) and \eqn{\tau}{tau} (shape2) as well as rate parameter
#'  \eqn{\lambda}{lambda}.
#'
#' @templateVar kappa TRUE
#' @template burr-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' VaR_burr(kappa = .8, rate = 2, shape1 = 2, shape2 = 5)
#'
#' # With rate parameter
#' VaR_burr(kappa = .8, scale = 0.5, shape1 = 2, shape2 = 5)
#'
VaR_burr <- function(kappa, shape1, shape2, rate = 1 / scale, scale = 1 / rate) {
    (rate * ((1 - kappa)^(-1/shape1) - 1))^(1/shape2)
}
