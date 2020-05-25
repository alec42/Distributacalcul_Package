#' Variance of the Erlang distribution
#'
#' @description Variance of the Erlang distribution
#'  with shape parameter \eqn{n} and rate parameter \eqn{\beta}{beta}.
#'
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
V_erlang <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape %% 1 == 0, rate > 0, shape > 0)

    shape / rate^2
}
