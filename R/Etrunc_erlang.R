#' Truncated mean of the Erlang distribution
#'
#' @description Truncated mean of the Erlang distribution
#'  with shape parameter \eqn{n} and rate parameter \eqn{\beta}{beta}.
#'
#' @template d-template
#' @template less.than.d-template
#' @template erlang-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' Etrunc_erlang(d = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' Etrunc_erlang(d = 2, shape = 2, rate = 0.2)
#'
#' # Values greater than d
#' Etrunc_erlang(d = 2, shape = 2, rate = 0.2, less.than.d = FALSE)
#'
Etrunc_erlang <- function(d, shape, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(d >= 0, shape %% 1 == 0, rate > 0, shape > 0)

    if (less.than.d) {
        Etrunc.gamma <- E_gamma(shape, rate) * stats::pgamma(q = d, shape = shape + 1, rate = rate)
    } else {
        Etrunc.gamma <- E_gamma(shape, rate) * stats::pgamma(q = d, shape = shape + 1, rate = rate, lower.tail = FALSE)
    }

    return(Etrunc.gamma)
}
