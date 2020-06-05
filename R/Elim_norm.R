#' Limited mean of the Normal distribution
#'
#' @description Limited mean of the Normal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar d TRUE
#' @template norm-template
#'
#' @export
#' @importFrom stats dnorm pnorm
#'
#' @examples
#'
#' Elim_norm(d = 2, mean = 2, sd = 5)
#'
Elim_norm <- function(d, mean = 0, sd = 1) {
    stopifnot(sd > 0)

    mean *
        stats::pnorm(q = d, mean = mean, sd = sd) -
        sd^2 *
        stats::dnorm(x = d, mean = mean, sd = sd) +
        d *
        stats::pnorm(q = d, mean = mean, sd = sd, lower.tail = FALSE)
}
