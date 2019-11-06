#' Variance of the Erlang Distribution
#'
#' @description Variance of the Erlang distribution with shape parameter \eqn{n}
#'  and rate parameter \eqn{\beta}{b}.
#'
#' @template erlang-template
#'
#' @seealso
#'  [derlang()] for the probability mass function,
#'  [perlang()] for the cumulative probability mass function,
# [TVaR_erlang()] for the Tail Value-at-Risk,
#'  [Mexcess_erlang()] for the Mean Excess Loss,
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
#' V_erlang(shape = 2, scale = 5)
#'
#' # With rate parameter
#' V_erlang(shape = 2, rate = 0.2)
#'
V_erlang <- function(shape, scale, rate = 1 / scale)
{
    shape / rate^2
}
