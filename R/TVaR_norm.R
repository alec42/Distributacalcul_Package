#' Tail Value-at-Risk of the Normal distribution
#'
#' @description Tail Value-at-Risk of the Normal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar q FALSE
#' @templateVar kappa TRUE
#' @template norm-template
#'
#' @export
#'
#' @examples
#'
#' TVaR_norm(kappa = 0.8, mean = 2, sd = 5)
#'
TVaR_norm <- function(kappa, mean = 0, sd = 1) {
    fact_tvar <- 1 / (1 - kappa)
    exposant <- (qnorm(p = kappa, mean = mean, sd = sd))^2
    mean + fact_tvar * sd * (1 / sqrt(2 * pi)) * exp(-exposant/2)
}


