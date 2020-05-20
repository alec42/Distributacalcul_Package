#' Tail Value-at-Risk of the Normal distribution
#'
#' @description Tail Value-at-Risk of the Normal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar kap TRUE
#' @template norm-template
#'
#' @export
#' @importFrom stats qnorm dnorm
#'
#' @examples
#'
#' TVaR_norm(kap = 0.8, mean = 2, sd = 5)
#'
TVaR_norm <- function(kap, mean = 0, sd = 1) {
    stopifnot(kap >= 0, kap < 1, sd > 0)

    mean +
        (sd / (1 - kap)) *
        stats::dnorm(x = stats::qnorm(p = kap))
}


