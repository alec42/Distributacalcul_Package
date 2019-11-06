#' Cumulative Density Function of the Erlang Distribution
#'
#' @description Cumulative density function of the Erlang distribution with shape
#'  parameter \eqn{n} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar q TRUE
#' @templateVar kappa FALSE
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
perlang <- function(q, shape, scale = 1 / rate, rate = 1 / scale, lower.tail = T)
{
    Fx <- exp(-rate * q) *
        sum(sapply(0:(shape - 1), function(j) ((rate * q)^j) / factorial(j)))

    if (lower.tail == T) {
        (1 - Fx)
    } else {
        Fx
    }
}
