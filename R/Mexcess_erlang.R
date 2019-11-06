#' Mean Excess Loss of the Erlang Distribution
#'
#' @description Mean excess loss of the Erlang distribution with shape
#'  parameter \eqn{n} and rate parameter \eqn{\beta}{b}.
#'
#' @templateVar d TRUE
#' @template erlang-template
#'
#' @seealso
#'  [derlang()] for the probability mass function,
#'  [perlang()] for the cumulative probability mass function,
#'  [V_erlang()] for the variance,
# [TVaR_erlang()] for the Tail Value-at-Risk,
#'  [SL_erlang()] for the stop-loss,
#'  [Elim_erlang()] for the limited expected value,
#'  [Etronq_erlang()] for the truncated mean,
#'  [E_erlang()] for the expected value, and
#'  [kthmoment_erlang()] for the \eqn{k}th moment.
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' Mexcess_erlang(d = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' Mexcess_erlang(d = 2, shape = 2, rate = 0.2)
#'
Mexcess_erlang <- function(d, shape, scale, rate = 1 / scale)
{
    (shape / rate) *
        pgamma(q = d, shape = shape + 1, rate = rate, lower.tail = F) /
        pgamma(q = d, shape = shape, rate = rate, lower.tail = F) -
        d
}
