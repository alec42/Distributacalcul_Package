#' Truncated mean of the Erlang distribution
#'
#' @description Truncated mean of the Erlang distribution with shape parameter
#'  \eqn{n} and rate parameter \eqn{\beta}{beta}.
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
#' Etronq_erlang(d = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' Etronq_erlang(d = 2, shape = 2, rate = 0.2)
#'
Etronq_erlang <- function(d, shape, scale = 1 / rate, rate = 1 / scale)
{
    stopifnot(d >= 0, shape %% 1 == 0, rate > 0)

    shape/rate * (1 - exp(-rate * d) *
                      sum(sapply(0:shape, function(j) ((rate * d)^j)/factorial(j))))
}
