#' Tail Value-at-Risk of the Erlang distribution
#'
#' @description Tail Value-at-Risk of the Erlang distribution with shape
#'  parameter \eqn{n} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar kap TRUE
#' @templateVar vark TRUE
#' @templateVar q FALSE
#' @template erlang-template
#'
#' @details This formula needs the formula for the VaR_erlang (already installed with the tvarPackage).
#'
# @export
.TVaR_erlang <- function(kap, shape, scale = 1 / rate, rate = 1 / scale, vark)
{
    stopifnot(kap >= 0, kap <= 1, shape %% 1 == 0, rate > 0)

    (shape / ((1 - kap) * rate)) *
        (exp(-rate * vark) *
             sum(sapply(0:shape, function(j) ((rate * vark)^j)/factorial(j)))
         )
}
