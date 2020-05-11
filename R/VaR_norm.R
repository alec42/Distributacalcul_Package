#' Value-at-Risk of the Normal distribution
#'
#' @description Value-at-Risk of the Normal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar q FALSE
#' @templateVar kap TRUE
#' @template norm-template
#'
#' @export
#'
#' @examples
#'
#' VaR_norm(kap = 0.8, mean = 3, sd = 5)
#'
VaR_norm <- function(kap, mean = 0, sd = 1) {
    stats::qnorm(p = kap, mean = mean, sd = sd)
}


