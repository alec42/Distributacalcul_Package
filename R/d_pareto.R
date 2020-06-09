#' Density function of the Pareto distribution
#'
#' @description Density function of the Pareto distribution
#'  with shape parameter \eqn{\alpha}{alpha} and rate parameter
#'  \eqn{\lambda}{lambda}.
#'
#' @template x-template
#' @template pareto-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' d_pareto(x = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' d_pareto(x = 2, shape = 2, rate = 5)
#'
d_pareto <- function(x, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(x >= 0, shape > 1, rate > 0)

    shape * rate^shape / (rate + x)^(shape + 1)
}
