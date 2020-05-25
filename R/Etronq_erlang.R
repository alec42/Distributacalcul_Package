#' Truncated mean of the Erlang distribution
#'
#' @description Truncated mean of the Erlang distribution
#'  with shape parameter \eqn{n} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @templateVar less.than.d TRUE
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
#' # Values greater than d
#' Etronq_erlang(d = 2, shape = 2, rate = 0.2, less.than.d = FALSE)
#'
Etronq_erlang <- function(d, shape, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(d >= 0, shape %% 1 == 0, rate > 0, shape > 0)

    if (less.than.d) {
        Etronq.gamma <- E_gamma(shape, rate) * stats::pgamma(q = d, shape = shape + 1, rate = rate)
    } else {
        Etronq.gamma <- E_gamma(shape, rate) * stats::pgamma(q = d, shape = shape + 1, rate = rate, lower.tail = FALSE)
    }

    return(Etronq.gamma)
}
