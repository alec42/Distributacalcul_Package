#' Stop-loss of the Pareto distribution
#'
#' @description Stop-loss of the Pareto distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\lambda}{lambda}.
#'
#' @templateVar d TRUE
#' @template pareto-template
#'
#' @export
#' @importFrom actuar ppareto
#'
#' @examples
#'
#' # With scale parameter
#' SL_pareto(d = 2, shape = 5, rate = 2)
#'
#' # With rate parameter
#' SL_pareto(d = 2, shape = 5, scale = 0.5)
#'
SL_pareto <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 1, rate > 0, d >= 0)

    E_pareto(shape, rate) * actuar::ppareto(q = d, shape = shape - 1, scale = rate, lower.tail = FALSE)
}
