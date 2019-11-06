#' kth Moment of the Erlang Distribution
#'
#' @description kth-moment of the Erlang distribution with shape parameter \eqn{n}
#'  and rate parameter \eqn{\beta}{b}.
#'
#' @templateVar k TRUE
#' @template erlang-template
#'
#' @seealso
#'  [derlang()] for the probability mass function,
#'  [perlang()] for the cumulative probability mass function,
#'  [V_erlang()] for the variance,
# [TVaR_erlang()] for the Tail Value-at-Risk,
#'  [Mexcess_erlang()] for the Mean Excess Loss,
#'  [Elim_erlang()] for the limited expected value,
#'  [Etronq_erlang()] for the truncated mean, and
#'  [E_erlang()] for the expected value.
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
kthmoment_erlang <- function(k, shape, scale, rate = 1 / scale)
{
    prod(sapply(0:(k - 1), function(i) (shape + i))) /
        (rate^k)
}
