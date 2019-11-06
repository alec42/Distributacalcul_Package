#' Density Function of the Erlang Distribution
#'
#' @description Density function of the Erlang distribution with shape
#'  parameter \eqn{n} and rate parameter \eqn{\beta}{b}.
#'
#' @templateVar x TRUE
#' @templateVar lower.tail TRUE
#' @template erlang-template
#'
#' @seealso
#'  [perlang()] for the cumulative probability mass function,
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
#' derlang(x = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' derlang(x = 2, shape = 2, rate = 0.2)
#'
derlang <- function(x, shape, scale, rate = 1 / scale)
{
    ((rate^shape) / gamma(shape)) *
        (x^(shape - 1)) *
        exp(-rate * x)
}
