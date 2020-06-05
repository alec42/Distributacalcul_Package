#' Moment Generating Function of the Erlang distribution
#'
#' @description Moment Generating Function (MGF) of the Erlang distribution
#'  with shape parameter \eqn{n}{n} and rate parameter \eqn{\beta}{beta}.
#'
#' @param t t.
#' @template erlang-template
#'
#' @export
#'
#' @examples
#'
#' MGF_erlang(t = 1, shape = 3, rate = 5)
#'
MGF_erlang <- function(t, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape %% 1 == 0, shape > 0, rate > 0, t < rate) # domain for t where non-neg?

    (rate / (rate - t))^shape
}
