#' Variance of the Pareto distribution
#'
#' @description Variance of the Pareto distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\lambda}{lambda}.
#'
#' @template pareto-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' V_pareto(shape = 5, rate = 2)
#'
#' # With rate parameter
#' V_pareto(shape = 5, scale = 0.5)
#'
V_pareto <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 1, rate > 0)

    (shape * rate^2) / ((shape - 1)^2 * (shape - 2))
}
