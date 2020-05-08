#' Value-at-Risk of the Pareto distribution
#'
#' @description Value-at-Risk of the Pareto distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\lambda}{lambda}.
#'
#' @templateVar kap TRUE
#' @template pareto-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' VaR_pareto(kap = .99, shape = 5, rate = 2)
#'
#' # With rate parameter
#' VaR_pareto(kap = .99, shape = 5, scale = 0.5)
#'
VaR_pareto <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0, kap >= 0, kap <= 1)

    rate * ((1 - kap)^(-1 / shape) - 1)
}
