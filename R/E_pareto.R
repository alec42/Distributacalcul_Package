#' Expected value of the Pareto distribution
#'
#' @description Expected value of the Pareto distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\lambda}{lambda}.
#'
#' @template pareto-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' E_pareto(shape = 5, rate = 2)
#'
#' # With rate parameter
#' E_pareto(shape = 5, scale = 0.5)
#'
E_pareto <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 1, rate > 0)

    rate / (shape - 1)
}
