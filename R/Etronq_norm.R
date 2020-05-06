#' Truncated Mean of the Normal Distribution
#'
#' @description Truncated mean of the Normal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar d TRUE
#' @templateVar q FALSE
#' @templateVar kappa FALSE
#' @template norm-template
#'
#' @export
#'
#' @examples
#'
#' Etronq_norm(d = 2, mean = 2, sd = 5)
#'
Etronq_norm <- function(d, mean = 0, sd = 1) {
    phi1 <- (d - mean) / sd
    mean * pnorm(phi1) -
        (sd / sqrt(2*pi)) * exp(-phi1^2 / 2)
}
