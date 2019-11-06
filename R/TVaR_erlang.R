#' Tail Value-at-Risk of the Erlang Distribution
#'
#' @description Tail Value-at-Risk of the Erlang distribution with shape
#'  parameter \eqn{n} and rate parameter \eqn{\beta}{b}.
#'
#' @templateVar kappa TRUE
#' @templateVar vark TRUE
#' @template erlang-template
#'
#' @details Cette formule nécessite la formule de la VaR_erlang (déjà installée avec le package tvarPackage)
#'
#' @seealso
#'  [derlang()] for the probability mass function,
#'  [perlang()] for the cumulative probability mass function,
#'  [V_erlang()] for the variance,
#'  [Mexcess_erlang()] for the Mean Excess Loss,
#'  [SL_erlang()] for the stop-loss,
#'  [Elim_erlang()] for the limited expected value,
#'  [Etronq_erlang()] for the truncated mean,
#'  [E_erlang()] for the expected value, and
#'  [kthmoment_erlang()] for the \eqn{k}th moment.
#'
#' @export
.TVaR_erlang <- function(kappa, shape, scale, rate = 1 / scale, vark)
{
    (shape / ((1 - kappa) * rate)) *
        (exp(-rate * vark) *
             sum(sapply(0:shape, function(j) ((rate * vark)^j)/factorial(j)))
         )
}
