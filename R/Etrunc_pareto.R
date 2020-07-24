#' Truncated mean of the Pareto distribution
#'
#' @description Truncated mean of the Pareto distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\lambda}{lambda}.
#'
#' @template d-template
#' @template less.than.d-template
#' @template pareto-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' Etrunc_pareto(d = 4, shape = 5, rate = 2)
#'
#' # With rate parameter
#' Etrunc_pareto(d = 4, shape = 5, scale = 0.5)
#'
Etrunc_pareto <- function(d, shape, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(shape > 1, rate > 0, d >= 0)

    if (less.than.d) {
        Etrunc.pareto <- E_pareto(shape, rate) * ppareto(q = d, shape - 1, rate) -
            d * ppareto(q = d, shape, rate, lower.tail = FALSE)
    } else {
        Etrunc.pareto <- E_pareto(shape, rate) * ppareto(q = d, shape - 1, rate, lower.tail = FALSE) +
            d * ppareto(q = d, shape, rate, lower.tail = FALSE)
    }

    return(Etrunc.pareto)
}
