#' Mean excess loss of the Erlang distribution
#'
#' @description Mean excess loss of the Erlang distribution
#'  with shape parameter \eqn{n} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @template erlang-template
#'
#' @export
#' @importFrom stats pgamma
#'
#' @examples
#'
#' # With scale parameter
#' Mexcess_erlang(d = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' Mexcess_erlang(d = 2, shape = 2, rate = 0.2)
#'
Mexcess_erlang <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(d >= 0, shape %% 1 == 0, rate > 0, shape > 0)

    E_erlang(shape, rate) *
        stats::pgamma(q = d, shape = shape + 1, rate = rate, lower.tail = FALSE) /
        stats::pgamma(q = d, shape = shape, rate = rate, lower.tail = FALSE) -
        d
}
