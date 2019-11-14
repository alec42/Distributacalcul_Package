#' Density Function of the Erlang Distribution
#'
#' @description Density function of the Erlang distribution with shape
#'  parameter \eqn{n} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar x TRUE
#' @templateVar q FALSE
#' @templateVar kappa FALSE
#' @template erlang-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' derlang(x = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' derlang(x = 2, shape = 2, rate = 0.2)
#'
derlang <- function(x, shape, scale = 1 / rate, rate = 1 / scale)
{
    ((rate^shape) / gamma(shape)) *
        (x^(shape - 1)) *
        exp(-rate * x)
}
