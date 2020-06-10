#' Cumulative density function of the Loglogistic distribution
#'
#' @description Cumulative density function of the Loglogistic distribution
#'  with shape parameter \eqn{\tau}{tau} and scale parameter \eqn{\lambda}{lambda}.
#'
#' @template q-template
#' @template lower.tail-template
#' @template loglogistic-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' pllogis(q = 3, shape = 3, scale = 5)
#'
#' # With rate parameter
#' pllogis(q = 3, shape = 3, rate = 0.2)
#'
#' # Survival function
#' pllogis(q = 3, shape = 3, rate = 0.2, lower.tail = FALSE)
#'
pllogis <- function(q, shape, rate = 1 / scale, scale = 1 / rate, lower.tail = TRUE) {
    stopifnot(q >= 0, shape > 1, rate > 0)

    Sx <- rate^shape / (rate^shape + q^shape)
    return(ifelse(lower.tail, 1 - Sx, Sx))
}
