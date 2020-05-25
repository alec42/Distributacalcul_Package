#' kth moment of the Erlang distribution
#'
#' @description kth moment of the Erlang distribution
#'  with shape parameter \eqn{n} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar k TRUE
#' @template erlang-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' kthmoment_erlang(k = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' kthmoment_erlang(k = 2, shape = 2, rate = 0.2)
#'
kthmoment_erlang <- function(k, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape %% 1 == 0, rate > 0, shape > 0, k >= 1) # k ?

    prod(sapply(0:(k - 1), function(i) (shape + i))) /
        (rate^k)
}
