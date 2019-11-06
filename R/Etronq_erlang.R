#' Truncated Mean of the Erlang Distribution
#'
#' @description Truncated mean of the Erlang distribution with shape parameter
#'  \eqn{n} and rate parameter \eqn{\beta}{b}.
#'
#' @templateVar d TRUE
#' @template erlang-template
#'
#' @seealso
#'  [derlang()] for the probability mass function,
#'  [perlang()] for the cumulative probability mass function,
#'  [V_erlang()] for the variance,
# [TVaR_erlang()] for the Tail Value-at-Risk,
#'  [Mexcess_erlang()] for the Mean Excess Loss,
#'  [Elim_erlang()] for the limited expected value,
#'  [E_erlang()] for the expected value, and
#'  [kthmoment_erlang()] for the \eqn{k}th moment.
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' Etronq_erlang(d = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' Etronq_erlang(d = 2, shape = 2, rate = 0.2)
#'
Etronq_erlang <- function(d, shape, scale, rate = 1 / scale)
{
    shape/rate * (1 - exp(-rate * d) *
                      sum(sapply(0:shape, function(j) ((rate * d)^j)/factorial(j))))
}
