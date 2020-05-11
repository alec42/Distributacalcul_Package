#' kth moment of the Pareto distribution
#'
#' @description kth moment of the Pareto distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\lambda}{lambda}.
#'
#' @templateVar k TRUE
#' @template pareto-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' kthmoment_pareto(k = 4, shape = 5, rate = 2)
#'
#' # With rate parameter
#' kthmoment_pareto(k = 4, shape = 5, scale = 0.5)
#'
kthmoment_pareto <- function(k, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0, shape > k)

    if (k %% 1 == 0 && k > 0) {
        kthmoment.pareto <- (rate^k * factorial(k)) / prod(shape - seq(from = 1, to = k, by = 1))
    } else if (k > -1) {
        kthmoment.pareto <- (rate^k * gamma(k + 1) * gamma(shape - k))/gamma(shape)
    }

    return(kthmoment.pareto)
}
