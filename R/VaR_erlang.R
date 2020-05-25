#' Value-at-Risk of the Erlang distribution
#'
#' @description Value-at-Risk of the Erlang distribution with shape parameter
#'  \eqn{n}{n} and rate parameter \eqn{\beta}{beta}.
#'
#' @note Wrapper of qgamma from package stats.
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
#' VaR_erlang(kap = .2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' VaR_erlang(kap = .2, shape = 3, rate = 0.25)
#'
VaR_erlang <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(kap >= 0, kap < 1, shape > 0, rate > 0)

    stats::qgamma(p = kap, shape = shape, rate = rate)
}
