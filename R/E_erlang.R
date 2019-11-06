#' Expected Value of the Erlang Distribution
#'
#' @description Expected value of the Erlang distribution with shape parameter \eqn{n}
#'  and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar q FALSE
#' @templateVar kappa FALSE
#' @template erlang-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' E_erlang(shape = 2, scale = 5)
#'
#' # With rate parameter
#' E_erlang(shape = 2, rate = 0.2)
#'
E_erlang <- function(shape, scale = 1 / rate, rate = 1 / scale)
{
    shape / rate
}
