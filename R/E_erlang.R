#' Expected value of the Erlang distribution
#'
#' @description Expected value of the Erlang distribution
#'  with shape parameter \eqn{n} and rate parameter \eqn{\beta}{beta}.
#'
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
E_erlang <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape %% 1 == 0, rate > 0, shape > 0)

    shape / rate
}
