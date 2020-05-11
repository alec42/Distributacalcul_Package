#' Mean excess loss of the Pareto distribution
#'
#' @description Mean excess loss of the Pareto distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\lambda}{lambda}.
#'
#' @templateVar d TRUE
#' @template pareto-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' Mexcess_pareto(d = 6, shape = 5, rate = 2)
#'
#' # With rate parameter
#' Mexcess_pareto(d = 6, shape = 5, scale = 0.5)
#'
Mexcess_pareto <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 1, rate > 0, d >= 0)

    E_pareto(shape, rate + d)
}
