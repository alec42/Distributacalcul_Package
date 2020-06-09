#' Truncated mean of the Pareto distribution
#'
#' @description Truncated mean of the Pareto distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\lambda}{lambda}.
#'
#' @templateVar d TRUE
#' @templateVar less.than.d TRUE
#' @template pareto-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' Etronq_pareto(d = 4, shape = 5, rate = 2)
#'
#' # With rate parameter
#' Etronq_pareto(d = 4, shape = 5, scale = 0.5)
#'
Etronq_pareto <- function(d, shape, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(shape > 1, rate > 0, d >= 0)

    if (less.than.d) {
        Etronq.pareto <- E_pareto(shape, rate) * p_pareto(q = d, shape - 1, rate) -
            d * p_pareto(q = d, shape, rate, lower.tail = FALSE)
    } else {
        Etronq.pareto <- E_pareto(shape, rate) * p_pareto(q = d, shape - 1, rate, lower.tail = FALSE) +
            d * p_pareto(q = d, shape, rate, lower.tail = FALSE)
    }

    return(Etronq.pareto)
}
