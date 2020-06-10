#' Cumulative density function of the Pareto distribution
#'
#' @description Cumulative density function of the Pareto distribution
#'  with shape parameter \eqn{\alpha}{alpha} and rate parameter
#'  \eqn{\lambda}{lambda}.
#'
#' @template q-template
#' @template lower.tail-template
#' @template pareto-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' ppareto(q = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' ppareto(q = 2, shape = 2, rate = 5)
#'
#' # Survival function
#' ppareto(q = 2, shape = 2, rate = 5, lower.tail = FALSE)
#'
ppareto <- function(q, shape, rate = 1 / scale, scale = 1 / rate, lower.tail = TRUE) {
    stopifnot(q >= 0, shape > 1, rate > 0)

    Sx <- (rate / (rate + q))^shape

    return(ifelse(lower.tail, 1 - Sx, Sx))
}
