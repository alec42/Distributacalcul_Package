#' Cumulative density function of the Erlang distribution
#'
#' @description Cumulative density function of the Erlang distribution
#'  with shape parameter \eqn{n} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar q TRUE
#' @templateVar lower.tail TRUE
#' @template erlang-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' perlang(q = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' perlang(q = 2, shape = 2, rate = 0.2)
#'
perlang <- function(q, shape, rate = 1 / scale, scale = 1 / rate, lower.tail = TRUE) {
    stopifnot(q >= 0, shape %% 1 == 0, rate > 0, shape > 0)

    if (lower.tail) {
        FSx <- 1 - exp(-rate * q) *
            sum(sapply(0:(shape - 1), function(j) ((rate * q)^j) / factorial(j)))
    } else {
        FSx <- exp(-rate * q) *
            sum(sapply(0:(shape - 1), function(j) ((rate * q)^j) / factorial(j)))
    }

    return(FSx)
}
