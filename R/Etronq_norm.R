#' Truncated mean of the Normal distribution
#'
#' @description Truncated mean of the Normal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar d TRUE
#' @templateVar less.than.d TRUE
#' @template norm-template
#'
#' @export
#' @importFrom stats pnorm dnorm
#'
#' @examples
#'
#' Etronq_norm(d = 2, mean = 2, sd = 5)
#'
Etronq_norm <- function(d, mean = 0, sd = 1, less.than.d = TRUE) {
    stopifnot(sd > 0)

    if (less.than.d) {
        Etronq.norm <- mean * stats::pnorm(q = d, mean = mean, sd = sd) -
            sd^2 * stats::dnorm(x = d, mean = mean, sd = sd)
    } else {
        Etronq.norm <- mean * stats::pnorm(q = d, mean = mean, sd = sd, lower.tail = FALSE) +
            sd^2 * stats::dnorm(x = d, mean = mean, sd = sd)
    }

    return(Etronq.norm)
}
