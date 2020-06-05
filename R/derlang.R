#' Density function of the Erlang distribution
#'
#' @description Density function of the Erlang distribution
#'  with shape parameter \eqn{n} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar x TRUE
#' @template erlang-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' derlang(x = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' derlang(x = 2, shape = 2, rate = 0.2)
#'
derlang <- function(x, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(x >= 0, shape %% 1 == 0, rate > 0, shape > 0)

    ((rate^shape) / gamma(shape)) *
        (x^(shape - 1)) *
        exp(-rate * x)
}
