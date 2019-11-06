#' kth Moment of the Erlang Distribution
#'
#' @description kth-moment of the Erlang distribution with shape parameter \eqn{n}
#'  and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar k TRUE
#' @templateVar q FALSE
#' @templateVar kappa FALSE
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
kthmoment_erlang <- function(k, shape = 1 / rate, scale, rate = 1 / scale)
{
    prod(sapply(0:(k - 1), function(i) (shape + i))) /
        (rate^k)
}
