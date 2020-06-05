#' Mean excess loss of the Normal distribution
#'
#' @description Mean excess loss of the Normal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar d TRUE
#' @template norm-template
#'
#' @export
#' @importFrom stats pnorm dnorm
#'
#' @examples
#'
#' Mexcess_norm(d = 2, mean = 2, sd = 5)
#'
Mexcess_norm <- function(d, mean = 0, sd = 1) {
    stopifnot(sd > 0)

    mean +
        d -
        (sd^2) *
        stats::dnorm(x = d, mean = mean, sd = sd) /
        stats::pnorm(q = d, mean = mean, sd = sd, lower.tail = FALSE)
}
