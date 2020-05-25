#' Tail Value-at-Risk of the Erlang distribution
#'
#' @description Tail Value-at-Risk of the Erlang distribution
#'  with shape parameter \eqn{n} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar kap TRUE
#' @template erlang-template
#'
#' @export
#' @importFrom stats qgamma
#'
#' @examples
#'
#' # With scale parameter
#' TVaR_erlang(kap = .2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' TVaR_erlang(kap = .2, shape = 3, rate = 0.25)
#'
TVaR_erlang <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(kap >= 0, kap < 1, shape %% 1 == 0, rate > 0, shape > 0)

    vark <- stats::qgamma(p = kap, shape = shape, rate = rate)

    (E_erlang(shape, rate) / (1 - kap)) *
        exp(-rate * vark) *
        sum(sapply(0:shape, function(j) ((rate * vark)^j) / factorial(j)))
}
