#' Variance of the Normal distribution
#'
#' @description Variance of the Normal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @template norm-template
#'
#' @export
#'
#' @examples
#'
#' V_norm(mean = 3, sd = 5)
#'
V_norm <- function(mean, sd) {
    stopifnot(sd > 0)

    sd^2
}
