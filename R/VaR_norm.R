#' Value-at-Risk of the Normal distribution
#'
#' @description Value-at-Risk of the Normal distribution with mean
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
#' VaR_norm(kappa = 0.8, mean = 3, sd = 5)
#'
VaR_norm <- function(kappa, mean = 0, sd = 1) {
    qnorm(p = kappa, mean = mean, sd = sd)
}


