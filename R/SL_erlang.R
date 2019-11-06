#' Stop-Loss of the Erlang Distribution
#'
#' @description Stop-loss of the Erlang distribution with shape
#'  parameter \eqn{n} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @templateVar q FALSE
#' @templateVar kappa FALSE
#' @template erlang-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' SL_erlang(d = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' SL_erlang(d = 2, shape = 2, rate = 0.2)
#'
SL_erlang <- function(d, shape, scale = 1 / rate, rate = 1 / scale)
{
    (shape / rate) *
        pgamma(q = d, shape = shape + 1, rate = rate, lower.tail = F) -
        d *
        pgamma(q = d, shape = shape, rate = rate, lower.tail = F)
}
