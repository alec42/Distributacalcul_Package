#' Variance of the Erlang Distribution
#'
#' @description Variance of the Erlang distribution with shape parameter \eqn{n}
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
#' V_erlang(shape = 2, scale = 5)
#'
#' # With rate parameter
#' V_erlang(shape = 2, rate = 0.2)
#'
V_erlang <- function(shape, scale = 1 / rate, rate = 1 / scale)
{
    stopifnot(shape %% 2 == 0, rate > 0)

    shape / rate^2
}
