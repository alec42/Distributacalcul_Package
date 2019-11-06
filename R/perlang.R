#' Cumulative Density Function of the Erlang Distribution
#'
#' @description Cumulative density function of the Erlang distribution with shape
#'  parameter \eqn{n} and rate parameter \eqn{\beta}{b}.
#'
#' @templateVar q TRUE
#' @templateVar lower.tail TRUE
#' @template erlang-template
#'
#' @seealso
#'  [derlang()] for the probability mass function,
#'  [V_erlang()] for the variance,
#'  [Mexcess_erlang()] for the Mean Excess Loss,
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
#' perlang(q = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' perlang(q = 2, shape = 2, rate = 0.2)
#'
perlang <- function(q, shape, scale, rate = 1 / scale, lower.tail = T)
{
    Fx <- exp(-rate * x) *
        sum(sapply(0:(shape - 1), function(j) ((rate * x)^j) / factorial(j)))

    if (lower.tail == T) {
        (1 - Fx)
    } else {
        Fx
    }
}
