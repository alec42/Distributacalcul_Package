#' Tail Value-at-Risk of the Erlang Distribution
#'
#' @description Tail Value-at-Risk of the Erlang distribution with shape
#'  parameter \eqn{n} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar kappa TRUE
#' @templateVar vark TRUE
#' @templateVar q FALSE
#' @template erlang-template
#'
#' @details Cette formule nécessite la formule de la VaR_erlang (déjà installée avec le package tvarPackage)
#'
#' @export
.TVaR_erlang <- function(kappa, shape, scale = 1 / rate, rate = 1 / scale, vark)
{
    (shape / ((1 - kappa) * rate)) *
        (exp(-rate * vark) *
             sum(sapply(0:shape, function(j) ((rate * vark)^j)/factorial(j)))
         )
}
